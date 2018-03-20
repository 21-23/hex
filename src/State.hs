{-# LANGUAGE NamedFieldPuns #-}

module State where

import qualified Docker.Client as Docker

import ServiceIdentity (ServiceIdentity, ServiceType)

data ServiceRequest = ServiceRequest
  { from        :: ServiceIdentity
  , serviceType :: ServiceType
  }

data State = State
  { requestQueue :: [ServiceRequest]
  , containerIds :: [Docker.ContainerID]
  }

empty :: State
empty = State [] []

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
