{-# LANGUAGE FlexibleContexts #-}

import           System.Posix.Signals                     ( Handler(CatchOnce)
                                                          , installHandler
                                                          , sigINT
                                                          , sigTERM
                                                          )
import           Control.Concurrent                       ( MVar
                                                          , newMVar
                                                          , modifyMVar_
                                                          , readMVar
                                                          , threadDelay
                                                          )
import qualified Control.Concurrent.Async.Lifted.Safe
                                               as Async
import           Control.Concurrent.Async.Lifted.Safe     ( Async
                                                          , AsyncCancelled
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Catch                      ( MonadCatch
                                                          , handle
                                                          )
import           Control.Monad.Trans                      ( lift )
import           Control.Monad.Reader                     ( ReaderT
                                                          , MonadReader
                                                          , runReaderT
                                                          , mapReaderT
                                                          , ask
                                                          )

data Env = Env
  { messengerHost :: String
  , servicesCount :: MVar Int
  }

--
-- MonadCount
--
class Monad m => MonadCount m where
  updateCount :: (a -> a) -> MVar a -> m ()
  readCount :: MVar a -> m a

instance MonadCount IO where
  updateCount f mvar = modifyMVar_ mvar (pure . f)
  readCount = readMVar

instance MonadCount m => MonadCount (ReaderT r m) where
  updateCount f mvar = lift $ updateCount f mvar
  readCount = lift . readCount

--
-- MonadLogger
--
class Monad m => MonadLogger m where
  logInfo :: String -> m ()

instance MonadLogger IO where
  logInfo = putStrLn

instance MonadLogger m => MonadLogger (ReaderT r m) where
  logInfo = lift . logInfo

--
-- MonadAsync
--
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

printConfig :: (MonadLogger m, MonadReader Env m) => m ()
printConfig = messengerHost <$> ask >>= (\h -> logInfo $ "Config host: " ++ h)

printCount :: (MonadLogger m, MonadReader Env m, MonadCount m) => m ()
printCount = servicesCount <$> ask >>= readCount >>= logInfo . show

increaseCount
  :: (MonadLogger m, MonadAsync m, MonadReader Env m, MonadCount m) => m ()
increaseCount = do
  servicesCount <$> ask >>= updateCount (+ 1)
  async printCount >>= wait

counterApp
  :: ( MonadIO m
     , MonadCatch m
     , MonadCount m
     , MonadLogger m
     , MonadAsync m
     , MonadReader Env m
     )
  => m ()
counterApp = handle cancelHandler $ loop (0 :: Int)
 where
  loop count = if count >= 5
    then logInfo "Counter app is done."
    else do
      increaseCount
      liftIO $ threadDelay 1000000
      loop $ count + 1

cancelHandler :: (MonadLogger m, MonadReader Env m) => AsyncCancelled -> m ()
cancelHandler _ = do
  logInfo "Stopping conter app thread..."
  printConfig
  pure ()

app
  :: ( MonadIO m
     , MonadCatch m
     , MonadCount m
     , MonadLogger m
     , MonadAsync m
     , MonadReader Env m
     )
  => m ()
app = do
  printConfig
  printCount
  counterAppThread <- async counterApp
  liftIO $ setupInterruptionHandlers $ shutdown counterAppThread
  wait counterAppThread

shutdown :: (MonadAsync m, MonadLogger m) => Async a -> m ()
shutdown thread = do
  logInfo "Ctrl-C received..."
  cancel thread

setupInterruptionHandlers :: IO () -> IO ()
setupInterruptionHandlers handler = do
  _ <- installHandler sigINT (CatchOnce handler) Nothing
  _ <- installHandler sigTERM (CatchOnce handler) Nothing
  pure ()

main :: IO ()
main = do
  Env "localhost" <$> newMVar 0 >>= runReaderT app
  logInfo "Main thread is exiting"
