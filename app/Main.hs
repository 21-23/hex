{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml as Yaml
import qualified Docker.Client as Docker
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import System.Directory (makeAbsolute)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text

import HexFile (HexFile(HexFile), ServiceDefinition(ServiceDefinition))

main :: IO ()
main = do
  maybeHexFile <- Yaml.decodeFile "./Hexfile.yml" :: IO (Maybe HexFile)
  case maybeHexFile of
    Just (HexFile services entry) -> do
      httpHandler <- Docker.defaultHttpHandler
      Docker.runDockerT (Docker.defaultClientOpts { Docker.baseUrl = "http://127.0.0.1:2376" } , httpHandler) $
        case Map.lookup entry services of
          Nothing -> fail $ "Entry service '" <> show entry <> "' is not defined"
          Just (ServiceDefinition _ buildContext buildOptions _) -> do
            basePath <- liftIO $ makeAbsolute $ Text.unpack buildContext
            result <- Docker.buildImageFromDockerfile buildOptions basePath
            case result of
              Right _ -> do
                images <- Docker.listImages $ Docker.ListOpts True
                liftIO $ print images
              Left err -> liftIO $ print err


    Nothing -> putStrLn "Couldn't read Hexfile"
