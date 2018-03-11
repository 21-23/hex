{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Yaml as Yaml
import qualified Docker.Client as Docker
import Docker.Client.Types (Image(DockerImage))
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import System.Directory (makeAbsolute)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Network.WebSockets as WebSocket
import           Control.Exception (IOException, catch)
import           Control.Concurrent.Suspend.Lifted (msDelay, suspend)
import           Control.Monad (forever, when)
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import           Control.Concurrent.Async (async, wait)


import HexFile (HexFile(HexFile),
                ServiceDefinition(ServiceDefinition),
                MessengerDefinition(MessengerDefinition))
import Envelope (Envelope(Envelope), message)
import Message (IncomingMessage(Start), OutgoingMessage(CheckIn))
import Identity (Identity(Messenger, ContainerService))

app :: String -> Int -> WebSocket.ClientApp ()
app messengerHost messengerPort connection = do
  catch
    (WebSocket.sendTextData connection $ Aeson.encode $ Envelope Messenger (CheckIn ContainerService))
    (\exception -> putStrLn $ "Whoa " <> show (exception :: WebSocket.ConnectionException))
  forever $ do
    string <- catch
      (WebSocket.receiveData connection)
      (\exception -> do
        putStrLn $ "Error while receiving data" <> show (exception :: WebSocket.ConnectionException)
        suspend $ msDelay 500
        connectToMessenger messengerHost messengerPort $ app messengerHost messengerPort
        return "")
    case Aeson.eitherDecode string :: Either String (Envelope IncomingMessage) of
      Left err -> putStrLn err
      Right Envelope {message = (Start identity)} ->
        putStrLn $ "Requested to start " <> show identity

connectToMessenger :: String -> Int -> WebSocket.ClientApp () -> IO ()
connectToMessenger messengerHost messengerPort clientApp =
  catch
    (WebSocket.runClient messengerHost messengerPort "/" clientApp)
    (\exception -> do
      putStrLn $ "Connection to messenger lost: " <> show (exception :: WebSocket.ConnectionException)
      suspend $ msDelay 500
      putStrLn "Reconnecting..."
      connectToMessenger messengerHost messengerPort clientApp)

runServiceContainer :: ServiceDefinition -> Docker.DockerT IO (Either Docker.DockerError ())
runServiceContainer (ServiceDefinition name imageName _ _ createOptions) = do
  liftIO $ putStrLn $ "Creating " <> Text.unpack name <> "..."
  createResult <- Docker.createContainer createOptions (Just imageName)
  case createResult of
    Left err -> fail $ show err
    Right containerId -> do
      liftIO $ putStrLn $ "Starting " <> Text.unpack name <> "..."
      Docker.startContainer Docker.defaultStartOpts containerId

ensureBuiltImage :: ServiceDefinition -> Docker.DockerT IO ()
ensureBuiltImage (ServiceDefinition _ imageName buildContext buildOptions _) = do
  listResult <- Docker.listImages $ Docker.ListOpts True
  case listResult of
    Left err -> fail $ show err
    Right images -> do
      let maybeImage = List.find (\DockerImage {Docker.imageRepoTags} ->
                             (imageName <> ":latest") `elem` imageRepoTags) images
      when (Maybe.isNothing maybeImage) $ do
        liftIO $ putStrLn $ "Service " <> show imageName <> " is not built, building..."
        basePath <- liftIO $ makeAbsolute $ Text.unpack buildContext
        result <- Docker.buildImageFromDockerfile buildOptions basePath
        case result of
          Right _  -> liftIO $ putStrLn "Build successful"
          Left err -> liftIO $ print err

stopAndRemove :: ServiceDefinition -> Docker.DockerT IO ()
stopAndRemove (ServiceDefinition _ imageName buildContext buildOptions _) = do
  listResult <- Docker.listContainers $ Docker.ListOpts True
  case listResult of
    Left err -> fail $ show err
    Right containers -> do
      let maybeContainer = List.find (\Docker.Container {Docker.containerImageName} ->
                             containerImageName == imageName) containers
      case maybeContainer of
        Just Docker.Container {Docker.containerId} -> do
          liftIO $ putStrLn $ "A container for " <> show imageName <> " already exists, stopping & removing..."
          Docker.stopContainer Docker.DefaultTimeout containerId
          result <- Docker.deleteContainer Docker.defaultDeleteOpts containerId
          case result of
            Right _  -> liftIO $ putStrLn "Cleaned up successfully"
            Left err -> liftIO $ print err
        Nothing -> return ()

main :: IO ()
main = do
  decodedHexFile <- Yaml.decodeFileEither "./Hexfile.yml" :: IO (Either ParseException HexFile)
  case decodedHexFile of
    Right (HexFile services (MessengerDefinition messengerName messengerPort) entryServiceName) -> do
      httpHandler <- Docker.defaultHttpHandler
      Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
        case Map.lookup messengerName services of
          Nothing -> fail $ "Messenger service " <> show messengerName <> " is not defined"
          Just messengerDefinition@(ServiceDefinition name imageName buildContext buildOptions createOptions) -> do
            ensureBuiltImage messengerDefinition
            stopAndRemove messengerDefinition
            runServiceContainer messengerDefinition
            let messengerHost = "localhost"
            wsClient <- liftIO $ async $ connectToMessenger messengerHost messengerPort $ app messengerHost messengerPort
            case Map.lookup entryServiceName services of
              Just entryServiceDefinition -> do
                ensureBuiltImage entryServiceDefinition
                stopAndRemove entryServiceDefinition
                runServiceContainer entryServiceDefinition
                liftIO $ wait wsClient
              Nothing -> fail $ "Entry service " <> show entryServiceName <> " is not defined"

    Left err -> putStrLn $ "Error reading Hexfile: " <> show err
