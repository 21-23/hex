{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module WebSocketControl
  ( requestService
  , requestShutdown
  )
where

import           ServiceIdentity                          ( ServiceType
                                                            ( SandboxService
                                                            , ContainerService
                                                            )
                                                          , ServiceIdentity
                                                          , ServiceSelector
                                                            ( Messenger
                                                            , AnyOfType
                                                            )
                                                          )
import           Envelope                                 ( Envelope(Envelope)
                                                          , message
                                                          )
import           Game                                     ( Game
                                                            ( LodashQuickDraw
                                                            )
                                                          )
import           Data.Aeson                    as Aeson
                                                          ( FromJSON(parseJSON)
                                                          , Value
                                                            ( Object
                                                            , String
                                                            )
                                                          , (.:)
                                                          , ToJSON(toJSON)
                                                          , object
                                                          , (.=)
                                                          , encode
                                                          , eitherDecode
                                                          )
import           Control.Monad                            ( mzero )
import           Data.Semigroup                           ( (<>) )
import           Data.Text                                ( unpack )
import qualified Network.WebSockets            as WebSocket

type UnsignedMessage = ServiceIdentity -> Envelope OutgoingMessage

newtype IncomingMessage = CheckedIn ServiceIdentity

instance FromJSON IncomingMessage where
  parseJSON (Object message) = do
    name <- message .: "name"
    case name of
      String "checkedIn"       -> CheckedIn <$> message .: "identity"
      String badName           -> fail $ "Unrecognized message: " <> unpack badName
      _                        -> fail "Message name is not a string"
  parseJSON _ = mzero

data OutgoingMessage
  = CheckIn ServiceType
  | ServiceRequest ServiceType ServiceIdentity
  | Shutdown ServiceIdentity

instance ToJSON OutgoingMessage where
  toJSON (CheckIn serviceType) = object
    [ "name"     .= String "checkin"
    , "identity" .= serviceType
    ]
  toJSON (ServiceRequest serviceType identity) = object
    [ "name" .= String "service.request"
    , "type" .= serviceType
    , "from" .= identity
    ]
  toJSON (Shutdown identity) = object
    [ "name" .= String "shutdown"
    , "from" .= identity
    ]

requestService :: String -> Int -> ServiceType -> IO ()
requestService host port = send host port . ServiceRequest

requestShutdown :: String -> Int -> IO ()
requestShutdown host port = send host port Shutdown

send :: String -> Int -> (ServiceIdentity -> OutgoingMessage) -> IO ()
send host port = runWebsocketApp . signAndSend . addressToContainer
 where
  runWebsocketApp = WebSocket.runClient host port "/"
  addressToContainer msg = Envelope (AnyOfType ContainerService) . msg

signAndSend :: UnsignedMessage -> WebSocket.ClientApp ()
signAndSend msg conn = send checkin >> getSignature >>= send . msg
 where
  checkin = Envelope Messenger . CheckIn . SandboxService $ LodashQuickDraw
  send    = WebSocket.sendTextData conn . Aeson.encode
  getSignature =
    Aeson.eitherDecode <$> WebSocket.receiveData conn >>= \case
      Left  err -> fail err
      Right Envelope { message = CheckedIn identity } -> return identity
