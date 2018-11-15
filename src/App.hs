{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module App
  ( Env(Env)
  , ensureNetworking
  , ensureBuiltImage
  , getMessengerDefinition
  , runServiceContainer
  )
where

import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                          ( Map )
import qualified Data.Set                      as Set
import qualified Data.Maybe                    as Maybe
import qualified Data.List                     as List
import           Data.Semigroup                           ( (<>) )


import           Control.Monad                            ( void )
import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Except                     ( ExceptT(ExceptT)
                                                          , MonadError
                                                          , runExceptT
                                                          , catchError
                                                          , throwError
                                                          )
import           Control.Monad.Reader                     ( ReaderT
                                                          , Reader
                                                          , MonadReader
                                                          , runReaderT
                                                          , mapReaderT
                                                          , ask
                                                          )


import           Control.Monad.Async                      ( MonadAsync
                                                          , async
                                                          , wait
                                                          )
import           Control.Monad.Docker                     ( MonadDocker
                                                          , DockerResult
                                                          , dockerError
                                                          , listImages
                                                          , buildImageFromDockerfile
                                                          , pullImage
                                                          , listContainers
                                                          , createContainer
                                                          , startContainer
                                                          , stopContainer
                                                          , deleteContainer
                                                          , createNetwork
                                                          )
import           Control.Monad.Logger                     ( MonadLogger
                                                          , logInfo
                                                          , logError
                                                          )
import           Control.Monad.WebSocket                  ( MonadWebSocket
                                                          , ClientApp
                                                          , runClient
                                                          , sendTextData
                                                          , receiveData
                                                          )


import qualified Network.HTTP.Types            as HTTP
import qualified Docker.Client                 as Docker
import           Docker.Client                            ( DockerError )


import qualified HexFile
import           HexFile                                  ( ServiceName
                                                          , ServiceDefinition
                                                            ( ServiceDefinition
                                                            )
                                                          , BuildContext
                                                            ( DockerFile
                                                            , Image
                                                            )
                                                          )
data Env = Env
  { envServiceDefinitions :: Map ServiceName ServiceDefinition
  , envMessengerDefinition :: ServiceDefinition
  , envMessengerHost :: String
  , envMessengerPort :: Int
  }

websocketApp :: (MonadWebSocket m, MonadLogger m) => ClientApp m ()
websocketApp conn = do
  -- send checkin
  -- wait for commands
  logInfo "Websocket starts listening..."
  sendTextData conn ("Hello, Echo!" :: Text)
  resp <- receiveData conn
  logInfo "Got response:"
  logInfo $ show (resp :: Text)
  logInfo "waiting a bit..."

runWebSocketApp :: (MonadLogger m, MonadReader Env m, MonadWebSocket m) => m ()
runWebSocketApp = do
  host <- envMessengerHost <$> ask
  port <- envMessengerPort <$> ask
  let path = "/"
  logInfo "runWebSocketApp"
  -- TODO: error handling and reconnect
  runClient host port path websocketApp

getServiceDefinitions
  :: (MonadReader Env m) => m (Map ServiceName ServiceDefinition)
getServiceDefinitions = envServiceDefinitions <$> ask

getMessengerDefinition :: (MonadReader Env m) => m ServiceDefinition
getMessengerDefinition = envMessengerDefinition <$> ask

-- | ensures network is created for all services defined
ensureNetworking :: (MonadReader Env m, MonadDocker m) => DockerResult m ()
ensureNetworking =
  (getServiceDefinitions >>= mapM_ createNetworkWithName . allNetworkNames)
    `catchError` ignoreConflictError
 where
  -- if network already exist, Docker will respond with "409 Conflict" status
  ignoreConflictError e@(Docker.DockerInvalidStatusCode status)
    | HTTP.statusCode status == 409 = lift . pure $ ()
    | otherwise                     = throwError e
  ignoreConflictError e = throwError e

  allNetworkNames = Set.fromList . concatMap serviceNetworkNames . Map.elems

  serviceNetworkNames =
    HashMap.keys
      . Docker.endpointsConfig
      . Docker.networkingConfig
      . HexFile.createOptions

  createNetworkWithName name =
    createNetwork $ (Docker.defaultCreateNetworkOpts name)
      { Docker.createNetworkCheckDuplicate = True
      , Docker.createNetworkInternal       = False
      }

buildImage
  :: (MonadLogger m, MonadDocker m) => ServiceDefinition -> DockerResult m ()
buildImage (ServiceDefinition _ imageName buildContext buildOptions _) =
  case buildContext of
    DockerFile path -> do
      logInfo $ "Building " <> show imageName <> "..."
      -- TODO: makeAbsolute path when parsing HexFile
      -- basePath <- liftIO $ makeAbsolute $ Text.unpack path
      let basePath = Text.unpack path
      buildImageFromDockerfile buildOptions basePath
    Image _ -> do -- name and imageName seems to be the same thing
      logInfo $ "Pulling " <> show imageName <> "..."
      void $ pullImage imageName "latest" -- `void` to skip docker logs

ensureBuiltImage
  :: (MonadDocker m, MonadLogger m) => ServiceDefinition -> DockerResult m ()
ensureBuiltImage service@(ServiceDefinition _ imageName _ _ _) =
  listAllImages >>= when imageNotFound (buildImage service)
 where
  imageNotFound = Maybe.isNothing . List.find taggedImage
  taggedImage image = imageTag `elem` Docker.imageRepoTags image
  imageTag = imageName <> ":latest"
  when f a x = if f x then a else pure ()

cleanupContainers :: (MonadDocker m) => ServiceDefinition -> DockerResult m ()
cleanupContainers (ServiceDefinition _ imageName _ _ _) =
  removeContainersByImage imageName

createServiceContainer
  :: (MonadDocker m) => ServiceDefinition -> DockerResult m Docker.ContainerID
createServiceContainer (ServiceDefinition _ imageName _ _ createOptions) =
  createContainer createOptions (Just imageName)

startServiceContainer
  :: (MonadDocker m) => Docker.ContainerID -> DockerResult m ()
startServiceContainer = startContainer Docker.defaultStartOpts

-- | prepares and starts service container
runServiceContainer
  :: (MonadDocker m, MonadLogger m) => ServiceDefinition -> DockerResult m ()
runServiceContainer service = do
  ensureBuiltImage service
  cleanupContainers service
  createServiceContainer service >>= startServiceContainer

--
-- utility
--

listAllImages :: (MonadDocker m) => DockerResult m [Docker.Image]
listAllImages = listImages $ Docker.defaultListOpts { Docker.all = True }

listAllContainers :: (MonadDocker m) => DockerResult m [Docker.Container]
listAllContainers =
  listContainers $ Docker.defaultListOpts { Docker.all = True }

removeContainersByImage :: (MonadDocker m) => Text -> DockerResult m ()
removeContainersByImage imageName =
  filterBy imageName <$> listAllContainers >>= mapM_ stopAndRemove
 where
  filterBy image = List.filter $ (== image) . Docker.containerImageName
  stopAndRemove = stopAndRemoveContainer . Docker.containerId

stopAndRemoveContainer
  :: (MonadDocker m) => Docker.ContainerID -> DockerResult m ()
stopAndRemoveContainer =
  deleteContainer Docker.defaultContainerDeleteOpts { Docker.force = True }
