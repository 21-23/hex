{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module HexFile where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Docker.Client (BuildOpts, ContainerConfig, defaultBuildOpts, defaultContainerConfig, buildDockerfileName)
import Data.Aeson (FromJSON(parseJSON), (.:), Value(Object))
import Control.Monad (mzero)
import Data.Semigroup ((<>))

data ServiceDefinition = ServiceDefinition
  { name            :: Text
  , buildContext    :: Text
  , buildOptions    :: BuildOpts
  , containerConfig :: ContainerConfig
  }

instance FromJSON ServiceDefinition where
  parseJSON (Object definition) = do
    name <- definition .: "name"
    buildContext <- definition .: "context"
    let buildOpts = defaultBuildOpts $ "hex_" <> name
    return $ ServiceDefinition name
                               buildContext
                               buildOpts
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
