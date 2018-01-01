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
import           Network.Socket (withSocketsDo)
import           Control.Exception (IOException, catch)
import           Control.Concurrent.Suspend.Lifted (msDelay, suspend)
import           Control.Monad (forever)
import qualified Data.Aeson as Aeson


import HexFile (HexFile(HexFile),
                ServiceDefinition(ServiceDefinition),
                MessengerDefinition(MessengerDefinition))
import Envelope (Envelope(Envelope), message)
import Message (IncomingMessage(Start), OutgoingMessage(CheckIn))
import Identity (Identity(Messenger, MetaService))

app :: String -> Int -> WebSocket.ClientApp ()
app messengerHost messengerPort connection = do
  WebSocket.sendTextData connection $ Aeson.encode $ Envelope Messenger (CheckIn MetaService)
  forever $ do
    string <- catch
      (WebSocket.receiveData connection)
      (\exception -> do
        print (exception :: WebSocket.ConnectionException)
        suspend $ msDelay 500
        connectToMessenger messengerHost messengerPort $ app messengerHost messengerPort
        return "")
    case Aeson.eitherDecode string :: Either String (Envelope IncomingMessage) of
      Left err -> putStrLn err
      Right Envelope {message} -> case message of
        Start identity -> putStrLn $ "Requested to start " <> show identity

connectToMessenger :: String -> Int -> WebSocket.ClientApp () -> IO ()
connectToMessenger messengerHost messengerPort clientApp =
  catch
    (withSocketsDo $ WebSocket.runClient messengerHost messengerPort "/" clientApp)
    (\exception -> do
      print (exception :: WebSocket.ConnectionException)
      suspend $ msDelay 500
      connectToMessenger messengerHost messengerPort clientApp)

main :: IO ()
main = do
  decodedHexFile <- Yaml.decodeFileEither "./Hexfile.yml" :: IO (Either ParseException HexFile)
  case decodedHexFile of
    Right (HexFile services (MessengerDefinition messengerName messengerPort) entry) -> do
      httpHandler <- Docker.defaultHttpHandler
      Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
        case Map.lookup messengerName services of
          Nothing -> fail $ "Messenger service '" <> show messengerName <> "' is not defined"
          Just (ServiceDefinition name imageName buildContext buildOptions createOptions) -> do
            listResult <- Docker.listImages $ Docker.ListOpts True
            case listResult of
              Left err -> fail $ show err
              Right images -> do
                let messengerImage = List.find (\DockerImage {Docker.imageRepoTags} ->
                                       (imageName <> ":latest") `elem` imageRepoTags) images
                case messengerImage of
                  Just _ -> do
                    createResult <- Docker.createContainer createOptions (Just imageName)
                    case createResult of
                      Left err -> fail $ show err
                      Right containerId -> do
                        Docker.startContainer Docker.defaultStartOpts containerId
                        let messengerHost = "localhost"
                        liftIO $ connectToMessenger messengerHost messengerPort $ app messengerHost messengerPort

                  Nothing -> do
                    liftIO $ putStrLn $ "Messenger service '" <> show messengerName <> "' is not built"
                    basePath <- liftIO $ makeAbsolute $ Text.unpack buildContext
                    result <- Docker.buildImageFromDockerfile buildOptions basePath
                    case result of
                      Right _ -> do
                        images <- Docker.listImages $ Docker.ListOpts True
                        liftIO $ print images
                      Left err -> liftIO $ print err
      return ()




    Left err -> putStrLn $ "Error reading Hexfile: " <> show err
