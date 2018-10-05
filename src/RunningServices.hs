module RunningServices
  ( RunningServices
  , empty
  , addRequest
  , fulfillRequest
  , getContainerIDs
  )
where

import           Data.Maybe                               ( fromJust )
import           Data.Text                                ( Text )
import qualified Data.Set                      as Set
import           Data.Set                                 ( Set )

import qualified Docker.Client                 as Docker
import           ServiceIdentity                          ( ServiceIdentity
                                                          , ServiceType
                                                          )

type RunningServices = Set RunningService

data Status = Requested | Running deriving (Eq, Show)

data RunningService = RunningService
  { status :: Status
  , serviceType :: ServiceType
  , containerID :: ContainerID
  , _requestedBy :: ServiceIdentity
  }
  deriving (Eq, Show)

instance Ord RunningService where
  compare a b = compare (containerID a) (containerID b)

-- it's not possible to derive Ord instance for Docker.Client.ContainerID
-- so we have to redefine it
newtype ContainerID = ContainerID Text
  deriving (Eq, Ord, Show)

fromDockerCID :: Docker.ContainerID -> ContainerID
fromDockerCID = ContainerID . Docker.fromContainerID

toDockerCID :: ContainerID -> Docker.ContainerID
toDockerCID (ContainerID cid) = fromJust $ Docker.toContainerID cid

empty :: RunningServices
empty = Set.empty

addRequest
  :: ServiceIdentity
  -> ServiceType
  -> Docker.ContainerID
  -> RunningServices
  -> RunningServices
addRequest si st cid = Set.insert rs
 where
  rs = RunningService
    { status       = Requested
    , serviceType  = st
    , containerID  = fromDockerCID cid
    , _requestedBy = si
    }

-- NOTE: will set `Running` status to all matching service types
-- TODO: identify services by something more reliable, like docker container id
fulfillRequest :: ServiceType -> RunningServices -> RunningServices
fulfillRequest st = Set.map setRunning
 where
  setRunning s = if serviceType s == st then s { status = Running } else s

getContainerIDs :: RunningServices -> [Docker.ContainerID]
getContainerIDs = fmap (toDockerCID . containerID) . Set.toList
