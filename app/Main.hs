{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml as Yaml
import qualified Docker.Client as Docker
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import System.Directory (makeAbsolute)
import Control.Monad.IO.Class (liftIO)

import HexFile (HexFile(HexFile), ServiceDefinition(ServiceDefinition))

main :: IO ()
main = do
  maybeHexFile <- Yaml.decodeFile "./Hexfile.yml" :: IO (Maybe HexFile)
  case maybeHexFile of
    Just (HexFile services entry) -> do
      httpHandler <- Docker.defaultHttpHandler
      result <- Docker.runDockerT (Docker.defaultClientOpts, httpHandler) $
        case Map.lookup entry services of
          Nothing -> fail $ "Entry service '" <> show entry <> "' is not defined"
          Just (ServiceDefinition _ buildOptions _) -> do
            basePath <- liftIO $ makeAbsolute "."
            Docker.buildImageFromDockerfile buildOptions basePath
      case result of
        Right _ -> putStrLn "Yay!"
        Left err -> print err

    Nothing -> putStrLn "Couldn't read Hexfile"
