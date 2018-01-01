{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Envelope where

import Control.Monad (mzero)
import Data.Aeson    (Value(Object), FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=), object)
import Message       (IncomingMessage, OutgoingMessage)
import Identity      (Identity, parseIdentity)

data Envelope a = Envelope
  { to :: Identity
  , message :: a
  }

instance ToJSON (Envelope OutgoingMessage) where
  toJSON Envelope { to, message } = object [
    "to" .= show to,
    "message" .= message
    ]

instance FromJSON (Envelope IncomingMessage) where
  parseJSON (Object envelope) = do
    to <- envelope .: "to"
    case parseIdentity to of
      Just identity -> Envelope <$> pure identity <*> envelope .: "message"
      Nothing       -> fail "Unknown identity"
  parseJSON _ = mzero
