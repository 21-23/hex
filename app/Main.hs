{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Functor (($>))
import qualified Data.HashMap.Strict as HashMap
import qualified Docker.Client as Docker
import Docker.Client.Types (Image(DockerImage))
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import System.Directory (makeAbsolute)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Network.WebSockets as WebSocket
import           Control.Exception (catch)
import           Control.Concurrent.Suspend.Lifted (msDelay, suspend)
import           Control.Monad (forever, when)
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import           Control.Concurrent (MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar, modifyMVar_)
import           Control.Concurrent.Async (async)
import           Data.Foldable (for_)
import           System.Posix.Signals       (Handler (CatchOnce),
                                             installHandler, sigINT, sigTERM)


import qualified HexFile
import HexFile (HexFile(HexFile),
                services,
                ServiceDefinition(ServiceDefinition),
                MessengerDefinition(MessengerDefinition),
                BuildContext(DockerFile, Image))
import Envelope (Envelope(Envelope), message)
import Message (IncomingMessage(ServiceRequest, ServiceCheckIn, Shutdown),
                OutgoingMessage(CheckIn, ServiceRequestFulfilled))
import ServiceIdentity (ServiceType(ContainerService),
                        ServiceSelector(Messenger, Service),
                        ServiceIdentity(ServiceIdentity))
import State (State)
import qualified State

app :: MVar State -> HexFile -> IO () -> WebSocket.ClientApp ()
app stateVar hexFile shutdownHandler connection = do
  -- save connection
  modifyMVar_ stateVar $ return . (State.setConnecton connection)
 
  catch
    (WebSocket.sendTextData connection $ Aeson.encode $ Envelope Messenger (CheckIn ContainerService))
    (\exception -> putStrLn $ "Whoa " <> show (exception :: WebSocket.ConnectionException))
  forever $ do
    string <- WebSocket.receiveData connection
    case Aeson.eitherDecode string :: Either String (Envelope IncomingMessage) of
      Left err -> putStrLn err
      Right Envelope{message} ->
        case message of
          ServiceRequest from serviceType -> do
            putStrLn $ "Requested to start " <> show serviceType
            state <- takeMVar stateVar
            let request = State.ServiceRequest from serviceType
            putMVar stateVar $ State.addRequest request state
            httpHandler <- Docker.defaultHttpHandler
            Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $ do
              let serviceName        = Text.pack $ show serviceType
                  HexFile {services} = hexFile
              case Map.lookup serviceName services of
                Nothing -> fail $ "Service " <> show serviceName <> " is not defined"
                Just serviceDefinition -> do
                  ensureBuiltImage serviceDefinition
                  ensureNetworking serviceDefinition
                  runServiceContainer stateVar serviceDefinition
            return ()

          ServiceCheckIn serviceIdentity@(ServiceIdentity serviceType _) -> do
            putStrLn $ "Got a checkin message from " <> show serviceIdentity
            state <- takeMVar stateVar
            case State.fulfillRequest serviceType state of
              Just (State.ServiceRequest identity newServiceIdentity, newState) -> do
                putMVar stateVar newState
                WebSocket.sendTextData connection $ Aeson.encode $
                  Envelope (Service identity) (ServiceRequestFulfilled serviceIdentity)
              Nothing -> putMVar stateVar state

          Shutdown -> async shutdownHandler $> ()
            
connectToMessenger :: MVar State -> String -> Int -> WebSocket.ClientApp () -> IO ()
connectToMessenger stateVar messengerHost messengerPort clientApp =
  catch
    (WebSocket.runClient messengerHost messengerPort "/" clientApp)
    exceptionHandler
  where
    exceptionHandler e@(WebSocket.CloseRequest _ _) = reconnectUnlessShutdown e
    exceptionHandler e@(WebSocket.ConnectionClosed) = reconnectUnlessShutdown e
    exceptionHandler e = reconnect e

    reconnectUnlessShutdown exception = do
      ws <- State.websocket <$> readMVar stateVar
      case ws of
        State.ConnectionClosed -> return ()
        _ -> reconnect exception

    reconnect exception = do
      putStrLn $ "Websocket error: " <> show (exception :: WebSocket.ConnectionException)
      suspend $ msDelay 500
      putStrLn "Reconnecting..."
      connectToMessenger stateVar messengerHost messengerPort clientApp

runServiceContainer :: MVar State -> ServiceDefinition -> Docker.DockerT IO (Either Docker.DockerError ())
runServiceContainer stateVar (ServiceDefinition name imageName buildContext _ createOptions) = do
  liftIO $ putStrLn $ "Creating " <> Text.unpack name <> "..."
  createResult <- Docker.createContainer createOptions Nothing
  case createResult of
    Left err -> fail $ show err
    Right containerId -> do
      state <- liftIO $ takeMVar stateVar
      liftIO $ putMVar stateVar $ State.addContainerId containerId state
      liftIO $ putStrLn $ "Starting " <> Text.unpack name <> "..."
      Docker.startContainer Docker.defaultStartOpts containerId

ensureNetworking :: ServiceDefinition -> Docker.DockerT IO ()
ensureNetworking (ServiceDefinition _ _ _ _ (Docker.CreateOpts _ _ networkingConfig)) =
  mapM_ createNetworkWithName networkNames
  where
    networkNames = HashMap.keys $ Docker.endpointsConfig networkingConfig
    createNetworkWithName name = do
      Docker.createNetwork $
        (Docker.defaultCreateNetworkOpts name)
        { Docker.createNetworkCheckDuplicate = True
        , Docker.createNetworkInternal = False
        }

ensureBuiltImage :: ServiceDefinition -> Docker.DockerT IO ()
ensureBuiltImage (ServiceDefinition _ imageName buildContext buildOptions _) = do
  listResult <- Docker.listImages $ Docker.ListOpts True
  case listResult of
    Left err -> fail $ show err
    Right images -> do
      let findImage img = List.find (\DockerImage {Docker.imageRepoTags} ->
                                      (img <> ":latest") `elem` imageRepoTags) images
      case buildContext of
        DockerFile path ->
          when (Maybe.isNothing $ findImage imageName) $ do
            liftIO $ putStrLn $ "Service " <> show imageName <> " is not built, building..."
            basePath <- liftIO $ makeAbsolute $ Text.unpack path
            Docker.buildImageFromDockerfile buildOptions basePath
            return ()
        Image name ->
          when (Maybe.isNothing $ findImage name) $
            liftIO $ putStrLn $ "Service image " <> show name <> " is not pulled"

stopAndRemove :: ServiceDefinition -> Docker.DockerT IO ()
stopAndRemove (ServiceDefinition _ name buildContext buildOptions _) = do
  listResult <- Docker.listContainers $ Docker.ListOpts True
  case listResult of
    Left err -> fail $ show err
    Right containers -> do
      let findContainer = List.find (\Docker.Container {Docker.containerNames} ->
                             ("/" <> name) `elem` containerNames) containers
      case findContainer of
        Just Docker.Container {Docker.containerId} -> do
          liftIO $ putStrLn $ "A container for " <> show name <> " already exists, stopping & removing..."
          Docker.stopContainer Docker.DefaultTimeout containerId
          result <- Docker.deleteContainer Docker.defaultDeleteOpts containerId
          case result of
            Right _  -> liftIO $ putStrLn "Cleaned up successfully"
            Left err -> liftIO $ print err
        Nothing -> return ()

shutdown :: MVar State -> ExitFlag -> IO ()
shutdown stateVar exitFlag = do
  putStrLn "Shutting down..."

  -- close websocket connection
  modifyMVar_ stateVar $ \state -> do
    case State.websocket state of
      State.Connected connection -> do
        WebSocket.sendClose connection ("Hex is shutting down. See ya, Arnaux" :: Text.Text)
        return $ State.setConnectionClosed state
      _ -> return state
    
  -- kill containers
  httpHandler <- Docker.defaultHttpHandler
  state <- readMVar stateVar
  Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
    for_ (State.containerIds state) stopAndRemove
  putStrLn "All containers have been stopped!"

  -- tell the main thread that it's time to exit
  setExitFlag exitFlag
  where
    stopAndRemove containerId = do
      liftIO $ putStrLn $ "Stopping container " <> show containerId <> "..."
      Docker.stopContainer Docker.DefaultTimeout containerId
      liftIO $ putStrLn $ "Stopped successfully, now removing the container " <> show containerId <> "..."
      result <- Docker.deleteContainer Docker.defaultDeleteOpts containerId
      case result of
        Left err -> liftIO $ print err
        Right _  -> liftIO $ putStrLn $ "Cleaned up successfully: " <> show containerId

setupInterruptionHandlers :: IO () -> IO Handler
setupInterruptionHandlers handler = do
  installHandler sigINT (CatchOnce handler) Nothing
  installHandler sigTERM (CatchOnce handler) Nothing

type ExitFlag = MVar ()

newExitFlag :: IO ExitFlag
newExitFlag = newEmptyMVar

setExitFlag :: ExitFlag -> IO ()
setExitFlag flag = putMVar flag ()

waitForExitFlag :: ExitFlag -> IO ()
waitForExitFlag = takeMVar

main :: IO ()
main = do
  stateVar <- newMVar State.empty
  exitFlag <- newExitFlag
  let shutdownHandler = shutdown stateVar exitFlag
  setupInterruptionHandlers shutdownHandler
  decodedHexFile <- HexFile.decode "./Hexfile.yml"
  case decodedHexFile of
    Right hexFile@(HexFile services (MessengerDefinition messengerName messengerPort) initSequence) -> do
      httpHandler <- Docker.defaultHttpHandler
      Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
        case Map.lookup messengerName services of
          Nothing -> fail $ "Messenger service " <> show messengerName <> " is not defined"
          Just messengerDefinition@(ServiceDefinition name imageName buildContext buildOptions createOptions) -> do
            ensureBuiltImage messengerDefinition
            ensureNetworking messengerDefinition
            runServiceContainer stateVar messengerDefinition
            let messengerHost = "localhost"
            wsClient <- liftIO $ async $ connectToMessenger stateVar messengerHost messengerPort $ app stateVar hexFile shutdownHandler
            for_ initSequence $ \serviceName ->
              case Map.lookup serviceName services of
                Just entryServiceDefinition -> do
                  ensureBuiltImage entryServiceDefinition
                  ensureNetworking entryServiceDefinition
                  runServiceContainer stateVar entryServiceDefinition
                Nothing -> fail $ "Init sequence: service " <> show serviceName <> " is not defined"

    Left err -> putStrLn $ "Error reading Hexfile: " <> show err

  waitForExitFlag exitFlag
