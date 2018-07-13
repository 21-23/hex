{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HexProcess
  ( whileRunning
  , runAndStop
  , withProcess
  , waitUntilServiceStarted
  )
where

import           Data.List                                ( isInfixOf )
import           Control.Monad                            ( MonadPlus, mzero )
import qualified System.Process.Typed          as TypedProcess
                                                          ( withProcess )
import           System.Process.Typed              hiding ( withProcess )
import           System.IO                                ( hGetContents
                                                          , hGetLine
                                                          , Handle
                                                          )
import           System.Directory                         ( makeAbsolute
                                                          , doesDirectoryExist
                                                          , findExecutable
                                                          )
import           System.Timeout                           ( timeout )
import           System.Process                           ( terminateProcess )
import           System.Exit                              ( ExitCode )


import           ServiceIdentity                          ( ServiceType )

type HexProcess = Process () Handle Handle

runAndStop :: IO ()
runAndStop = whileRunning $ return ()

whileRunning :: IO a -> IO a
whileRunning = withProcess . const

withProcess :: (HexProcess -> IO a) -> IO a
withProcess f = do
  hexConfig <- config <$> getExe <*> getWorkingDir
  TypedProcess.withProcess hexConfig $ \p -> do
    waitUntilHexIsStarted p
    f p <* stopIfNotStopped p
 where
  config exe dir = setStdout createPipe
    $ setStderr createPipe
    $ setWorkingDir dir
    $ proc exe []

  getExe = findExecutable "hex-exe" >>= \case
    Nothing     -> fail "hex-exe cannot be found"
    Just hexExe -> return hexExe

  getWorkingDir = do
    workDir       <- makeAbsolute "test/integration/fixtures"
    workDirExists <- doesDirectoryExist workDir
    if workDirExists
      then return workDir
      else fail $ workDir ++ " does not exists"

stopIfNotStopped :: HexProcess -> IO ExitCode
stopIfNotStopped p = getExitCode p >>= \case
  Nothing -> do
    terminateProcess $ unsafeProcessHandle p
    waitExitCode p
  Just exitCode -> return exitCode

waitUntilServiceStarted :: HexProcess -> ServiceType -> IO ()
waitUntilServiceStarted p service =
  unless
    (waitForMessage p $ "Started " ++ show service)
    (fail $ show service ++ " is not started within specified timeout")

waitUntilHexIsStarted :: HexProcess -> IO ()
waitUntilHexIsStarted p =
  unless
    (waitForMessage p "Init sequence complete!")
    (fail "cannot find init sequence in hex output")

waitForMessage :: HexProcess -> String -> IO Bool
waitForMessage p message = checkNextLine
  where
    checkNextLine = getLine >>= \case
      Nothing -> return False
      Just line ->
        if line == message
          then return True
          else checkNextLine
    getLine = timeout (10 * 1000 * 1000) $ hGetLine (getStdout p)

unless :: IO Bool -> IO () -> IO ()
unless mb error = mb >>= \b ->
  if b
    then return ()
    else error
