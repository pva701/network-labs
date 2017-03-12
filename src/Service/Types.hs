module Service.Types
       ( ExecState (..)
       , ExecMessage (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Data.Binary            (Binary (..))
import           Network.Socket         (SockAddr, Socket)
import           Universum

import           DNS.Types              (HostName, IPv4, HostMap)

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

data ExecMessage = FreeThreads !IPv4 !Int
    deriving Generic

instance Binary ExecMessage where
