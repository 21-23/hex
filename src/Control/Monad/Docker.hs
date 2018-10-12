module Control.Monad.Docker
  ( MonadDocker
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

class Monad m => MonadDocker m where
  listImages :: ListOpts -> m (Either DockerError [Image])
  buildImageFromDockerfile :: BuildOpts -> FilePath -> m (Either DockerError ())
  pullImage :: Text -> Tag -> m (Either DockerError ByteString)

  listContainers :: ListOpts -> m (Either DockerError [Container])
  createContainer :: CreateOpts -> Maybe ContainerName -> m (Either DockerError ContainerID)
  startContainer :: StartOpts -> ContainerID -> m (Either DockerError ())
  stopContainer :: Timeout -> ContainerID -> m (Either DockerError ())
  deleteContainer :: ContainerDeleteOpts -> ContainerID -> m (Either DockerError ())

  createNetwork :: CreateNetworkOpts -> m (Either DockerError NetworkID)

instance MonadDocker m => MonadDocker (ReaderT r m) where
  listImages = lift . listImages
  buildImageFromDockerfile opts path = lift $ buildImageFromDockerfile opts path
  pullImage name tag = lift $ pullImage name tag

  listContainers = lift . listContainers
  createContainer opts name = lift $ createContainer opts name
  startContainer opts cid = lift $ startContainer opts cid
  stopContainer timeout cid = lift $ stopContainer timeout cid
  deleteContainer opts cid = lift $ deleteContainer opts cid

  createNetwork = lift . createNetwork

instance (MonadIO m, MonadMask m) => MonadDocker (DockerT m) where
  listImages = Docker.listImages
  buildImageFromDockerfile = Docker.buildImageFromDockerfile
  pullImage name tag = Docker.pullImage name tag sinkLbs

  listContainers = Docker.listContainers
  createContainer = Docker.createContainer
  startContainer = Docker.startContainer
  stopContainer = Docker.stopContainer
  deleteContainer = Docker.deleteContainer

  createNetwork = Docker.createNetwork
