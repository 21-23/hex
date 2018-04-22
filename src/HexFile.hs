{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module HexFile where

import Data.Text (Text, splitOn, unpack)
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Docker.Client (BuildOpts, defaultBuildOpts,
                      CreateOpts(CreateOpts), defaultCreateOpts,
                      HostConfig, portBindings, networkMode, defaultHostConfig,
                      NetworkMode(NetworkNamed),
                      Port,
                      PortBinding(PortBinding), containerPort, portType, hostPorts,
                      HostPort(HostPort),
                      ExposedPort(ExposedPort),
                      PortType(TCP),
                      EnvVar(EnvVar),
                      ContainerConfig, exposedPorts, hostname, env, defaultContainerConfig,
                      EndpointConfig(EndpointConfig),
                      NetworkingConfig(NetworkingConfig), endpointsConfig,
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

data BuildContext
  = DockerFile Text
  | Image      Text

data ServiceDefinition = ServiceDefinition
  { name          :: ServiceName
  , imageName     :: Text
  , buildContext  :: BuildContext
  , buildOptions  :: BuildOpts
  , createOptions :: CreateOpts
  }

getHexName :: Text -> Text
getHexName string =
  let hexPrefix = "hex_"
      name = case splitOn ":" string of
               [serviceName, _] -> serviceName
               _                -> string
   in hexPrefix <> name

instance FromJSON ServiceDefinition where
  parseJSON (Object definition) = do
    name <- definition .: "name"
    mContext <- definition .:? "context"
    mImageName <- definition .:? "image"
    buildContext <- case (mContext, mImageName) of
                      (Just context, _)    -> return $ DockerFile context
                      (_, Just imageName)  -> return $ Image imageName
                      (_, _)               -> fail "Unrecognized build context"

    portMappings <- definition .:? "ports" .!= []
    envVars <- definition .:? "environment" .!= []

    let hexName       = getHexName name
        imageName     = case buildContext of
                          DockerFile _  -> hexName
                          Image imgName -> imgName
        buildOptions  = defaultBuildOpts hexName
        createOptions = let portBindings    = mappingToBinding <$> portMappings
                            exposedPorts    = mappingToExposedPort <$> portMappings
                            hostConfig      = defaultHostConfig
                                                { portBindings
                                                , networkMode = NetworkNamed "test-network"
                                                }
                            containerConfig = (defaultContainerConfig imageName)
                                                { exposedPorts
                                                , env = envKeyValueToEnvVar <$> envVars
                                                }
                            endpointsConfig = HashMap.fromList [("test-network", EndpointConfig [name])]
                            networkConfig   = NetworkingConfig {endpointsConfig}
                         in CreateOpts containerConfig hostConfig networkConfig
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
  { services     :: Map ServiceName ServiceDefinition
  , messenger    :: MessengerDefinition
  , initSequence :: [ServiceName]
  }

instance FromJSON HexFile where
  parseJSON (Object hexFile) = do
    serviceList <- hexFile .: "services"
    messenger <- hexFile .: "messenger"
    initSequence <- hexFile .: "init-sequence"
    let makeTuples definition@ServiceDefinition{name} = (name, definition)
        services = Map.fromList $ makeTuples <$> serviceList
    return $ HexFile services messenger initSequence
  parseJSON _ = mzero
