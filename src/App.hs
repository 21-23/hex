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
                                                          , ignoreDockerError
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
ensureNetworking
  :: (MonadReader Env m, MonadDocker m) => ExceptT Docker.DockerError m ()
ensureNetworking =
  (getServiceDefinitions >>= mapM_ createNetworkWithName . allNetworkNames)
    `catchError` ignoreConflictError
 where
  -- if network already exist, Docker will respond with "409 Conflict" status
  ignoreConflictError e@(Docker.DockerInvalidStatusCode status)
    | HTTP.statusCode status == 409 = lift . pure $ ()
    | otherwise                     = throwError e
  ignoreConflictError e = throwError e

  allNetworkNames :: Map ServiceName ServiceDefinition -> Set.Set Text
  allNetworkNames = Set.fromList . concatMap serviceNetworkNames . Map.elems

  serviceNetworkNames =
    HashMap.keys
      . Docker.endpointsConfig
      . Docker.networkingConfig
      . HexFile.createOptions

  createNetworkWithName
    -- :: (MonadDocker m) => Text -> m (Either Docker.DockerError Docker.NetworkID)
    :: (MonadDocker m) => Text -> ExceptT Docker.DockerError m Docker.NetworkID
  createNetworkWithName name =
    createNetwork $ (Docker.defaultCreateNetworkOpts name)
      { Docker.createNetworkCheckDuplicate = True
      , Docker.createNetworkInternal       = False
      }

-- TODO: think about overriding monad instance for `m (Either DockerError a)` and
-- provide this as >>= implementation
-- andThen :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
-- andThen mea fmeb = mea >>= either (return . Left) fmeb

-- listAllImages :: (MonadDocker m) => m (Either Docker.DockerError [Docker.Image])
listAllImages :: (MonadDocker m) => ExceptT Docker.DockerError m [Docker.Image]
listAllImages = listImages $ Docker.ListOpts True

buildImage
  :: (MonadLogger m, MonadDocker m)
  => ServiceDefinition
  -> ExceptT Docker.DockerError m ()
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
      void $ pullImage imageName "latest" -- skip docker logs

ensureBuiltImage
  :: (MonadDocker m, MonadLogger m)
  => ServiceDefinition
  -- -> m (Either Docker.DockerError ())
  -> ExceptT Docker.DockerError m ()
ensureBuiltImage service@(ServiceDefinition _ imageName _ _ _) =
  listAllImages >>= when imageNotFound (buildImage service)
 where
  imageNotFound = Maybe.isNothing . List.find taggedImage
  taggedImage image = imageTag `elem` Docker.imageRepoTags image
  imageTag = imageName <> ":latest"
  when f a x = if f x then a else pure ()

-- TODO: ExceptT
runServiceContainer
  :: (MonadDocker m, MonadLogger m)
  => ServiceDefinition
  -- -> m (Either Docker.DockerError ())
  -> ExceptT Docker.DockerError m ()
runServiceContainer s = do
  ensureBuiltImage s
  -- ensure image is built
  -- create container
  -- run container
