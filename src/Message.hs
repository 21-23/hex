{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson    (FromJSON(parseJSON),
                      Value(Object, String),
                      (.:),
                      ToJSON(toJSON),
                      object,
                      (.=)
                      )
import Control.Monad (mzero)
import Data.Monoid   ((<>))
import Data.Text     (Text)

import Identity      (Identity, parseIdentity)

newtype IncomingMessage
  = Start Identity

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "start" -> do
        identityString <- message .: "identity"
        case parseIdentity identityString of
          Just identity -> return $ Start identity
          Nothing       -> fail   $ "Unrecognized identity " <> identityString
      _                -> fail   "Message name is not a string"
  parseJSON _ = mzero

newtype OutgoingMessage
  = CheckIn Identity

instance ToJSON OutgoingMessage where
  toJSON (CheckIn identity) = object
    [ "name"     .= ("checkin" :: Text)
    , "identity" .= identity
    ]
