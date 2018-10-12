module Control.Monad.Logger
  ( MonadLogger
  , logInfo
  , logError
  )
where

import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT )
import           Docker.Client                            ( DockerT )

import           System.IO                                ( hPutStrLn
                                                          , stderr
                                                          )

class Monad m => MonadLogger m where
  logInfo :: String -> m ()
  logError :: String -> m ()

instance MonadLogger IO where
  logInfo = putStrLn
  logError = hPutStrLn stderr

instance MonadLogger m => MonadLogger (ReaderT r m) where
  logInfo = lift . logInfo
  logError = lift . logError

instance MonadLogger m => MonadLogger (DockerT m) where
  logInfo = lift . logInfo
  logError = lift . logError
