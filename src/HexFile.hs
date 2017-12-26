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

type ServiceName = Text

data ServiceDefinition = ServiceDefinition
  { name            :: ServiceName
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

data MessengerDefinition = MessengerDefinition
  { service :: ServiceName
  , port    :: Integer
  }

instance FromJSON MessengerDefinition where
  parseJSON (Object definition) = do
    service <- definition .: "service"
    port <- definition .: "port"
    return $ MessengerDefinition service port
  parseJSON _ = mzero

data HexFile = HexFile
  { services  :: Map Text ServiceDefinition
  , messenger :: MessengerDefinition
  , entry     :: [ServiceName]
  }

instance FromJSON HexFile where
  parseJSON (Object hexFile) = do
    serviceList <- hexFile .: "services"
    messenger <- hexFile .: "messenger"
    entry <- hexFile .: "entry"
    let makeTuples definition@ServiceDefinition{name} = (name, definition)
        services = Map.fromList $ makeTuples <$> serviceList
    return $ HexFile services messenger entry
  parseJSON _ = mzero
