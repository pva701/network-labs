{-# LANGUAGE TypeFamilies #-}

module DNS.Types
       ( DNSState (..)
       , IP

       , DNSServReqMsg (..)
       , DNSServRespMsg (..)
       , DNSClientReqMsg (..)
       , DNSClientRespMsg (..)

       , MonadDNS (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Trans    (MonadTrans (lift))
import           Data.ByteString        (ByteString)
import           Network.Socket         (HostName, SockAddr, Socket)
import           Universum

type IP = String
type HostMap = HashMap HostName IP

data DNSState = DNSState
    { activeHosts :: !(TVar HostMap)
    , sendSocket  :: !(Socket, SockAddr)
    , recvSocket  :: !Socket
    }

data DNSServReqMsg
    = DNSHello !HostName
    | DNSPing !HostName

data DNSServRespMsg = DNSOlleh !HostMap

data DNSClientReqMsg = DNSRequest !HostName
data DNSClientRespMsg = DNSResponseIP !IP | DNSResponseUnknown

class MonadIO m => MonadDNS m where
    myHost   :: m HostName
    askHosts :: m (TVar HostMap)

    sendMulticast :: ByteString -> m ()
    sendDirectly  :: ByteString -> SockAddr -> m ()
    recvMulticast :: m (ByteString, SockAddr)

    default askHosts
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => m (TVar HostMap)
    askHosts = lift askHosts

    default sendMulticast
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => ByteString -> m ()
    sendMulticast = lift . sendMulticast

    default recvMulticast
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => m (ByteString, SockAddr)
    recvMulticast = lift recvMulticast

