{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Docker
  ( MonadDocker
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
where

import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT )
import           Control.Monad.Except                     ( ExceptT(ExceptT)
                                                          , catchError
                                                          , throwError
                                                          , mapExceptT
                                                          )
import           Control.Monad.Catch                      ( MonadMask )
import           Control.Monad.IO.Class                   ( MonadIO )

import           Data.Semigroup                           ( (<>) )
import           Data.Text                                ( Text )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Conduit.Binary                      ( sinkLbs )

import qualified Docker.Client                 as Docker
import           Docker.Client                            ( DockerT
                                                          , DockerError
                                                          , ListOpts
                                                          , BuildOpts
                                                          , CreateOpts
                                                          , StartOpts
                                                          , ContainerDeleteOpts
                                                          , CreateNetworkOpts
                                                          , Tag
                                                          , Image
                                                          , Container
                                                          , ContainerName
                                                          , ContainerID
                                                          , Timeout
                                                          , NetworkID
                                                          )

-- ignoreDockerError :: Monad m => DockerResult m () -> DockerResult m ()
-- ignoreDockerError x = catchError x . const . lift . pure $ ()

type DockerResult m a = ExceptT DockerError m a

dockerError :: (Monad m) => Text -> DockerResult m a
dockerError = throwError . Docker.GenericDockerError

class Monad m => MonadDocker m where
  listImages :: ListOpts -> DockerResult m [Image]
  buildImageFromDockerfile :: BuildOpts -> FilePath -> DockerResult m ()
  pullImage :: Text -> Tag -> DockerResult m ByteString

  listContainers :: ListOpts -> DockerResult m [Container]
  createContainer :: CreateOpts -> Maybe ContainerName -> DockerResult m ContainerID
  startContainer :: StartOpts -> ContainerID -> DockerResult m ()
  stopContainer :: Timeout -> ContainerID -> DockerResult m ()
  deleteContainer :: ContainerDeleteOpts -> ContainerID -> DockerResult m ()

  createNetwork :: CreateNetworkOpts -> DockerResult m NetworkID

instance MonadDocker m => MonadDocker (ReaderT r m) where
  listImages = mapExceptT lift . listImages
  buildImageFromDockerfile opts path = mapExceptT lift $ buildImageFromDockerfile opts path
  pullImage name tag = mapExceptT lift $ pullImage name tag

  listContainers = mapExceptT lift . listContainers
  createContainer opts name = mapExceptT lift $ createContainer opts name
  startContainer opts cid = mapExceptT lift $ startContainer opts cid
  stopContainer timeout cid = mapExceptT lift $ stopContainer timeout cid
  deleteContainer opts cid = mapExceptT lift $ deleteContainer opts cid

  createNetwork = mapExceptT lift . createNetwork

instance (MonadIO m, MonadMask m) => MonadDocker (DockerT m) where
  -- listImages _ = dockerError "cannot list images"
  listImages = ExceptT . Docker.listImages
  -- buildImageFromDockerfile _ _ = dockerError "whoops, cannot build image"
  buildImageFromDockerfile opts path = ExceptT $ Docker.buildImageFromDockerfile opts path
  pullImage name tag = ExceptT $ Docker.pullImage name tag sinkLbs

  -- listContainers _ = dockerError "cannot list containers"
  listContainers = ExceptT . Docker.listContainers
  -- createContainer _ _ = dockerError "cannot create container"
  createContainer opts name = ExceptT $ Docker.createContainer opts name
  -- startContainer _ cid = dockerError $ "cannot start container " <> Docker.fromContainerID cid
  startContainer opts cid = ExceptT $ Docker.startContainer opts cid
  stopContainer timeout cid = ExceptT $ Docker.stopContainer timeout cid
  -- deleteContainer _ cid = dockerError $ "cannot delete container " <> Docker.fromContainerID cid
  deleteContainer opts cid = ExceptT $ Docker.deleteContainer opts cid

  -- createNetwork _ = dockerError "cannot create network"
  createNetwork = ExceptT . Docker.createNetwork
