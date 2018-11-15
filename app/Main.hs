{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup                           ( (<>) )
import           System.Exit                              ( exitFailure )
import           System.IO                                ( hSetBuffering
                                                          , stdout
                                                          , BufferMode
                                                            ( LineBuffering
                                                            )
                                                          )
import           Control.Monad.Except                     ( ExceptT
                                                          , runExceptT
                                                          )
import           Control.Monad.Reader                     ( ReaderT
                                                          , runReaderT
                                                          )
import qualified Docker.Client                 as Docker
import           Docker.Client                            ( DockerT
                                                          , runDockerT
                                                          )

import           Control.Monad.Logger                     ( logInfo
                                                          , logError
                                                          )


import qualified HexFile
import           HexFile                                  ( HexFile
                                                          , HexFileParseError
                                                          )
import           App                                      ( Env(Env)
                                                          , ensureNetworking
                                                          , getMessengerDefinition
                                                          , runServiceContainer
                                                          )

-- ensure networking for once
-- start messenger container
-- connect websocket
-- start init sequence

-- TODO: error handling: it would be great if we can automatically collect and
-- handle errors happening inside `do` block
app :: ExceptT Docker.DockerError (ReaderT Env (DockerT IO)) ()
app = do
  ensureNetworking
  getMessengerDefinition >>= runServiceContainer
  pure ()

forceLineBuffering :: IO ()
forceLineBuffering = hSetBuffering stdout LineBuffering

initialEnv :: HexFile -> IO Env
initialEnv hexFile = pure
  $ Env serviceDefinitions messengerDefinition messengerHost messengerPort
 where
  messenger           = HexFile.messenger hexFile
  messengerHost       = HexFile.messengerHost messenger
  messengerPort       = HexFile.messengerPort messenger
  messengerDefinition = HexFile.messengerService messenger
  serviceDefinitions  = HexFile.services hexFile

runApp :: HexFile -> IO ()
runApp hexFile = do
  forceLineBuffering -- needed by tests, they read output line by line
  env         <- initialEnv hexFile
  httpHandler <- Docker.unixHttpHandler "/var/run/docker.sock"
  -- httpHandler <- Docker.defaultHttpHandler
  result      <-
    runDockerT (Docker.defaultClientOpts, httpHandler)
    $ flip runReaderT env
    $ runExceptT app
  case result of
    Left  err -> logError $ show err
    Right _   -> logInfo "runApp is done"

exitWithParseError :: HexFileParseError -> IO ()
exitWithParseError err = do
  -- TODO: pretty parsing erorrs
  logError $ "Error reading Hexfile: " <> show err
  exitFailure

main :: IO ()
main = HexFile.parse "./Hexfile.yml" >>= either exitWithParseError runApp
