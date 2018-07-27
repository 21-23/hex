{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HexProcess
  ( whileRunning
  , runAndStop
  , withProcess
  , waitUntilServiceStarted
  )
where

import           GHC.Conc                                 ( STM
                                                          , atomically
                                                          )
import qualified Data.ByteString.Lazy.Char8    as L8
import           Data.List                                ( isInfixOf )
import           Control.Monad                            ( unless )
import           Control.Exception                        ( handle, throw, SomeException)
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

type HexProcess = Process () Handle (STM L8.ByteString)

runAndStop :: IO ()
runAndStop = whileRunning $ return ()

whileRunning :: IO a -> IO a
whileRunning = withProcess . const

withProcess :: (HexProcess -> IO a) -> IO a
withProcess f = do
  hexConfig <- config <$> getExe <*> getWorkingDir
  TypedProcess.withProcess hexConfig $ \p ->
    handle
      (checkStderr p)
      (waitUntilHexIsStarted p *> f p <* stopIfNotStopped p)

 where
  config exe dir = setStdout createPipe
    $ setStderr byteStringOutput
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

checkStderr :: HexProcess -> SomeException -> IO a
checkStderr p e = do
  stderr <- L8.unpack <$> atomically (getStderr p)
  if null stderr
    then throw e -- if stderr is empty, just rethrow error
    else failHex p stderr

stopIfNotStopped :: HexProcess -> IO ExitCode
stopIfNotStopped p = getExitCode p >>= \case
  Nothing -> do
    terminateProcess $ unsafeProcessHandle p
    waitExitCode p
  Just exitCode -> return exitCode

-- process must be stopped before calling `fail`, because TypedProcess
-- masks errors until process is done running, which may not be the case
-- (for example when we're failing because we couldn't find required message in
-- hex's output in time)
failHex :: HexProcess -> String -> IO a
failHex p e = stopIfNotStopped p *> fail e

waitUntilServiceStarted :: HexProcess -> ServiceType -> IO ()
waitUntilServiceStarted p service =
  unlessM
    (waitForMessage p $ "Started " ++ show service)
    (failHex p $ show service ++ " is not started within specified timeout")

waitUntilHexIsStarted :: HexProcess -> IO ()
waitUntilHexIsStarted p =
  unlessM
    (waitForMessage p "Init sequence complete!")
    (failHex p "cannot find init sequence in hex output")

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

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM b f = b >>= flip unless f
