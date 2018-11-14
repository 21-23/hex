{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Docker
  ( MonadDocker
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
where

import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT)
import           Control.Monad.Except                     ( ExceptT(ExceptT)
                                                          , catchError
                                                          , mapExceptT
                                                          )
import           Control.Monad.Catch                      ( MonadMask )
import           Control.Monad.IO.Class                   ( MonadIO )

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

ignoreDockerError :: Monad m => ExceptT DockerError m () -> ExceptT DockerError m ()
ignoreDockerError x = catchError x . const . lift . pure $ ()

class Monad m => MonadDocker m where
  listImages :: ListOpts -> ExceptT DockerError m [Image]
  buildImageFromDockerfile :: BuildOpts -> FilePath -> ExceptT DockerError m ()
  pullImage :: Text -> Tag -> ExceptT DockerError m ByteString

  listContainers :: ListOpts -> ExceptT DockerError m [Container]
  createContainer :: CreateOpts -> Maybe ContainerName -> ExceptT DockerError m ContainerID
  startContainer :: StartOpts -> ContainerID -> ExceptT DockerError m ()
  stopContainer :: Timeout -> ContainerID -> ExceptT DockerError m ()
  deleteContainer :: ContainerDeleteOpts -> ContainerID -> ExceptT DockerError m ()

  createNetwork :: CreateNetworkOpts -> ExceptT DockerError m NetworkID

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
  listImages = const $ ExceptT $ pure $ Left $ Docker.GenericDockerError "whops, cannot listImages"
  -- listImages = ExceptT . Docker.listImages
  -- buildImageFromDockerfile = ExceptT $ const $ const $ pure $ Left $ Docker.GenericDockerError "whoops, cannot build image"
  buildImageFromDockerfile opts path = ExceptT $ Docker.buildImageFromDockerfile opts path
  pullImage name tag = ExceptT $ Docker.pullImage name tag sinkLbs

  listContainers = ExceptT . Docker.listContainers
  createContainer opts name = ExceptT $ Docker.createContainer opts name
  startContainer opts cid = ExceptT $ Docker.startContainer opts cid
  stopContainer timeout cid = ExceptT $ Docker.stopContainer timeout cid
  -- deleteContainer = const $ pure $ Left $ Docker.GenericDockerError "whoooopsie in createNetwork"
  deleteContainer opts cid = ExceptT $ Docker.deleteContainer opts cid

  -- createNetwork = const $ ExceptT $ pure $ Left $ Docker.GenericDockerError "whoooopsie in createNetwork"
  createNetwork = ExceptT . Docker.createNetwork
