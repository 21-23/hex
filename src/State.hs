{-# LANGUAGE NamedFieldPuns #-}

module State where

import qualified Docker.Client as Docker

import ServiceIdentity (ServiceIdentity, ServiceType)
import qualified Network.WebSockets as WebSocket

data ServiceRequest = ServiceRequest
  { from        :: ServiceIdentity
  , serviceType :: ServiceType
  }

data State = State
  { requestQueue :: [ServiceRequest]
  , containerIds :: [Docker.ContainerID]
  , websocket :: WebSocket
  }

data WebSocket
  = NotConnected
  | Connected WebSocket.Connection
  | ConnectionClosed

empty :: State
empty = State [] [] NotConnected

addRequest :: ServiceRequest -> State -> State
addRequest request state@State{requestQueue} =
  state { requestQueue = requestQueue ++ [request] }

fulfillRequest :: ServiceType -> State -> Maybe (ServiceRequest, State)
fulfillRequest reqServiceType state@State{requestQueue} = findAndSplice [] requestQueue
  where
    findAndSplice _ [] = Nothing
    findAndSplice rs (request@ServiceRequest{serviceType} : rs')
      | serviceType == reqServiceType = Just (request, state { requestQueue = rs <> rs' })
      | otherwise                     = findAndSplice (rs <> [request]) rs'

addContainerId :: Docker.ContainerID -> State -> State
addContainerId containerId state@State{containerIds} =
  state { containerIds = containerIds ++ [containerId] }

setConnecton :: WebSocket.Connection -> State -> State
setConnecton connection state =
  state { websocket = Connected connection }

setConnectionClosed :: State -> State
setConnectionClosed state =
  state { websocket = ConnectionClosed }
