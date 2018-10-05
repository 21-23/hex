module RunningServices
  ( RunningServices
  , empty
  , addStarted
  , confirmRunning
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

data Status = Started | Running deriving (Eq, Show)

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

addStarted
  :: ServiceIdentity
  -> ServiceType
  -> Docker.ContainerID
  -> RunningServices
  -> RunningServices
addStarted si st cid = Set.insert rs
 where
  rs = RunningService
    { status       = Started
    , serviceType  = st
    , containerID  = fromDockerCID cid
    , _requestedBy = si
    }

-- NOTE: will set `Running` status to all matching service types
-- TODO: identify services by something more reliable, like docker container id
confirmRunning :: ServiceType -> RunningServices -> RunningServices
confirmRunning st =
  Set.map $ \s -> if serviceType s == st then s { status = Running } else s

getContainerIDs :: RunningServices -> [Docker.ContainerID]
getContainerIDs = fmap (toDockerCID . containerID) . Set.toList
