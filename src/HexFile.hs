{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module HexFile where

import Data.Text (Text, splitOn, unpack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Docker.Client (BuildOpts, defaultBuildOpts,
                      CreateOpts(CreateOpts), defaultCreateOpts,
                      HostConfig, portBindings, defaultHostConfig,
                      Port,
                      PortBinding(PortBinding), containerPort, portType, hostPorts,
                      HostPort(HostPort),
                      ExposedPort(ExposedPort),
                      PortType(TCP),
                      EnvVar(EnvVar),
                      ContainerConfig, exposedPorts, hostname, env, defaultContainerConfig,
                      buildDockerfileName)
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), (.!=), Value(Object, String))
import Control.Monad (mzero)
import Data.Semigroup ((<>))

type ServiceName = Text

data PortMapping = PortMapping
  { host      :: Port
  , container :: Port
  }

instance FromJSON PortMapping where
  parseJSON (String mapping) = do
    let [hostPort, containerPort] = read . unpack <$> splitOn ":" mapping
    return $ PortMapping hostPort containerPort
  parseJSON _ = mzero

mappingToBinding :: PortMapping -> PortBinding
mappingToBinding (PortMapping hostPort containerPort) =
  PortBinding { containerPort
              , portType  = TCP
              , hostPorts = [HostPort "0.0.0.0" hostPort]
              }

mappingToExposedPort :: PortMapping -> ExposedPort
mappingToExposedPort (PortMapping _ containerPort) =
  ExposedPort containerPort TCP

data EnvKeyValue = EnvKeyValue Text Text

instance FromJSON EnvKeyValue where
  parseJSON (String keyValue) = do
    let [key, value] = splitOn "=" keyValue
    return $ EnvKeyValue key value
  parseJSON _ = mzero

envKeyValueToEnvVar :: EnvKeyValue -> EnvVar
envKeyValueToEnvVar (EnvKeyValue key value) = EnvVar key value

data ServiceDefinition = ServiceDefinition
  { name          :: ServiceName
  , imageName     :: Text
  , buildContext  :: Text
  , buildOptions  :: BuildOpts
  , createOptions :: CreateOpts
  }

instance FromJSON ServiceDefinition where
  parseJSON (Object definition) = do
    name <- definition .: "name"
    buildContext <- definition .: "context"
    ports <- definition .:? "ports"
    envVars <- definition .:? "environment" .!= []

    let hexName       = "hex_" <> name
        buildOptions  = defaultBuildOpts hexName
        createOptions = case ports of
                          Nothing       -> defaultCreateOpts hexName
                          Just mappings ->
                            let portBindings    = mappingToBinding <$> mappings
                                hostConfig      = defaultHostConfig { portBindings }
                                exposedPorts    = mappingToExposedPort <$> mappings
                                containerConfig = (defaultContainerConfig hexName)
                                                    { exposedPorts
                                                    , hostname = Just name
                                                    , env = envKeyValueToEnvVar <$> envVars
                                                    }
                             in CreateOpts containerConfig hostConfig
    return $ ServiceDefinition name
                               hexName
                               buildContext
                               buildOptions
                               createOptions
  parseJSON _ = mzero

data MessengerDefinition = MessengerDefinition
  { service :: ServiceName
  , port    :: Int
  }

instance FromJSON MessengerDefinition where
  parseJSON (Object definition) = do
    service <- definition .: "service"
    port <- definition .: "port"
    return $ MessengerDefinition service port
  parseJSON _ = mzero

data HexFile = HexFile
  { services  :: Map ServiceName ServiceDefinition
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
