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
fulfillRequest reqServiceType state@State{requestQueue} =
  let (requests, fulfilled) = foldl (\(requests, fulfilled) request@ServiceRequest{serviceType} ->
                                      if serviceType == reqServiceType
                                        then (requests, Just request)
                                        else (requests ++ [request], fulfilled))
                                    ([], Nothing) requestQueue
   in (\request -> (request, state { requestQueue = requests })) <$> fulfilled

addContainerId :: Docker.ContainerID -> State -> State
addContainerId containerId state@State{containerIds} =
  state { containerIds = containerIds ++ [containerId] }

setConnecton :: WebSocket.Connection -> State -> State
setConnecton connection state =
  state { websocket = Connected connection }

setConnectionClosed :: State -> State
setConnectionClosed state =
  state { websocket = ConnectionClosed }
