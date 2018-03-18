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
import Data.Semigroup   ((<>))
import Data.Text     (Text, unpack)

import ServiceIdentity      (ServiceType, ServiceIdentity)

data IncomingMessage
  = ServiceRequest ServiceIdentity ServiceType
  | ServiceCheckIn ServiceIdentity

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "service.request" -> ServiceRequest <$> message .: "from" <*> message .: "type"
      String "checkedIn"       -> ServiceCheckIn <$> message .: "identity"
      String badName           -> fail $ "Unrecognized message: " <> unpack badName
      _                        -> fail "Message name is not a string"
  parseJSON _ = mzero

data OutgoingMessage
  = CheckIn ServiceType
  | ServiceRequestFulfilled ServiceIdentity

instance ToJSON OutgoingMessage where
  toJSON (CheckIn serviceType) = object
    [ "name"     .= String "checkin"
    , "identity" .= serviceType
    ]
  toJSON (ServiceRequestFulfilled identity) = object
    [ "name"     .= String "service.requestFulfilled"
    , "identity" .= identity
    ]
