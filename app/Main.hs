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

import HexFile (HexFile(HexFile),
                ServiceDefinition(ServiceDefinition),
                MessengerDefinition(MessengerDefinition))

main :: IO ()
main = do
  decodedHexFile <- Yaml.decodeFileEither "./Hexfile.yml" :: IO (Either ParseException HexFile)
  case decodedHexFile of
    Right (HexFile services (MessengerDefinition messengerName _) entry) -> do
      httpHandler <- Docker.defaultHttpHandler
      Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
        case Map.lookup messengerName services of
          Nothing -> fail $ "Messenger service '" <> show messengerName <> "' is not defined"
          Just (ServiceDefinition name buildContext buildOptions _) -> do
            listResult <- Docker.listImages $ Docker.ListOpts True
            case listResult of
              Left err -> fail $ show err
              Right images -> do
                let messengerImage = List.find (\DockerImage {Docker.imageRepoTags} ->
                                       (name <> ":latest") `elem` imageRepoTags) images
                case messengerImage of
                  Just _ -> do
                    createResult <- Docker.createContainer (Docker.defaultCreateOpts name) (Just name)
                    case createResult of
                      Left err -> fail $ show err
                      Right containerId ->
                        Docker.startContainer Docker.defaultStartOpts containerId
                  Nothing -> fail $ "Messenger service '" <> show messengerName <> "' is not built"
      return ()

        -- case Map.lookup entry services of
        --   Nothing -> fail $ "Entry service '" <> show entry <> "' is not defined"
        --   Just (ServiceDefinition _ buildContext buildOptions _) -> do
        --     basePath <- liftIO $ makeAbsolute $ Text.unpack buildContext
        --     result <- Docker.buildImageFromDockerfile buildOptions basePath
        --     case result of
        --       Right _ -> do
        --         images <- Docker.listImages $ Docker.ListOpts True
        --         liftIO $ print images
        --       Left err -> liftIO $ print err


    Left err -> putStrLn $ "Error reading Hexfile: " <> show err
