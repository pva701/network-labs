module Service.Types
       ( ExecState (..)
       , ExecMessage (..)
       , ConsumerState
       , ConsumerMessage (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Data.Binary            (Binary (..))
import           Network.Socket         (SockAddr, Socket)
import           Universum

import           DNS.Types              (HostMap, HostName, IPv4)

type FreeMap = Map IPv4 Int

data ExecState = ExecState
    { ownHttpHost      :: !HostName
    , httpPort         :: !Word16
    , ownIP            :: !IPv4
    , freeVar          :: !(TVar Int)
    , othersFreeVar    :: !(TVar FreeMap)
    , dnsHostsVar      :: !(TVar HostMap)
    , unicastSocket    :: !Socket
    , multicastSocket  :: !Socket
    , multicastAddress :: !SockAddr
    }

type ConsumerState = (Word16, TVar HostMap, TVar (Set IPv4))

data ConsumerMessage = ConsumerIP !IPv4
    deriving Generic

data ExecMessage = FreeThreads !IPv4 !Int
    deriving Generic

instance Binary ExecMessage where
instance Binary ConsumerMessage where
