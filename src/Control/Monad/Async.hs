module Control.Monad.Async
  ( MonadAsync
  , async
  , wait
  , cancel
  )
where

import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT
                                                          , mapReaderT
                                                          , ask
                                                          )
import           Docker.Client                            ( DockerT
                                                          , runDockerT
                                                          )

import qualified Control.Concurrent.Async.Lifted.Safe
                                               as Async
import           Control.Concurrent.Async.Lifted.Safe     ( Async )

class Monad m => MonadAsync m where
  async :: m a -> m (Async a)
  wait :: Async a -> m a
  cancel :: Async a -> m ()

instance MonadAsync IO where
  async = Async.async
  wait = Async.wait
  cancel = Async.cancel

instance MonadAsync m => MonadAsync (ReaderT r m) where
  async = mapReaderT async
  wait = lift . wait
  cancel = lift . cancel

instance MonadAsync m => MonadAsync (DockerT m) where
  async m = do
    conf <- ask
    lift . async $ runDockerT conf m
  wait = lift . wait
  cancel = lift . cancel
