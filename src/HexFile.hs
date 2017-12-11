{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module HexFile where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Docker.Client (BuildOpts, ContainerConfig, defaultBuildOpts, defaultContainerConfig, buildDockerfileName)
import Data.Aeson (FromJSON(parseJSON), (.:), Value(Object))
import Control.Monad (mzero)

data ServiceDefinition = ServiceDefinition
  { name            :: Text
  , buildOptions    :: BuildOpts
  , containerConfig :: ContainerConfig
  }

instance FromJSON ServiceDefinition where
  parseJSON (Object definition) = do
    name <- definition .: "name"
    dockerFileName <- definition .: "dockerFile"
    let buildOpts = defaultBuildOpts name
    return $ ServiceDefinition name
                               buildOpts { buildDockerfileName = dockerFileName }
                               (defaultContainerConfig name)
  parseJSON _ = mzero

data HexFile = HexFile
  { services :: Map Text ServiceDefinition
  , entry    :: Text
  }

instance FromJSON HexFile where
  parseJSON (Object hexFile) = do
    serviceList <- hexFile .: "services"
    entry <- hexFile .: "entry"
    let makeTuples definition@ServiceDefinition{name} = (name, definition)
        services = Map.fromList $ makeTuples <$> serviceList
    return $ HexFile services entry
  parseJSON _ = mzero
