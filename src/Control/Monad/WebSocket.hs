module Control.Monad.WebSocket
  ( MonadWebSocket
  , sendTextData
  , receiveData
  , runClient
  )
where

import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT
                                                          , runReaderT
                                                          , ask
                                                          )
import           Docker.Client                            ( DockerT
                                                          , runDockerT
                                                          )

import qualified Network.WebSockets            as WebSockets
import           Network.WebSockets                       ( Connection
                                                          , WebSocketsData
                                                          )

class Monad m => MonadWebSocket m where
  sendTextData :: WebSocketsData a => Connection -> a -> m ()
  receiveData :: WebSocketsData a => Connection -> m a
  runClient :: String -> Int -> String -> (Connection -> m a) -> m a

instance MonadWebSocket m => MonadWebSocket (ReaderT r m) where
  sendTextData conn d = lift $ sendTextData conn d
  receiveData = lift . receiveData
  runClient host port path app = do
    env <- ask
    lift $ runClient host port path (\conn -> runReaderT (app conn) env)

instance MonadWebSocket m => MonadWebSocket (DockerT m) where
  sendTextData conn d = lift $ sendTextData conn d
  receiveData = lift . receiveData
  runClient host port path app = do
    conf <- ask
    lift $ runClient host port path (\conn -> runDockerT conf (app conn))

instance MonadWebSocket IO where
  sendTextData = WebSockets.sendTextData
  receiveData = WebSockets.receiveData
  runClient = WebSockets.runClient
