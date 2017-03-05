{-# LANGUAGE TypeFamilies #-}

module DNS.Types
       ( DNSState (..)
       , IPv4

       , DNSServReqMsg (..)
       , DNSServRespMsg (..)
       , DNSClientReqMsg (..)
       , DNSClientRespMsg (..)

       , MonadDNS (..)
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Trans    (MonadTrans (lift))
import           Data.Binary            (Binary (..), getWord8, putWord8)
import           Data.ByteString.Lazy   (ByteString)
import           Network.Socket         (HostName, SockAddr, Socket)
import           Universum              hiding (ByteString)

type IPv4 = (Word8, Word8, Word8, Word8)
type HostMap = Map HostName IPv4

data DNSState = DNSState
    { activeHosts :: !(TVar HostMap)
    , pings       :: !(TVar HostMap)
    , ownHost     :: !HostName
    , sendSocket  :: !(Socket, SockAddr)
    , recvSocket  :: !Socket
    }

data DNSServReqMsg
    = DNSHello !HostName
    | DNSPing !HostName
    deriving (Generic)

data DNSServRespMsg
    = DNSOlleh !HostMap
    | DNSWrongHost
    deriving (Generic)

data DNSClientReqMsg = DNSRequest !HostName
    deriving (Generic)
data DNSClientRespMsg = DNSResponseIP !IPv4 | DNSResponseUnknown
    deriving (Generic)

instance Binary DNSServReqMsg where
    put (DNSHello x) = putWord8 0 >> put x
    put (DNSPing x)  = putWord8 1 >> put x
    get = do
        tag <- getWord8
        if | tag == 0 -> DNSHello <$> get
           | tag == 1 -> DNSPing <$> get
           | otherwise -> fail "Unexpected tag"

instance Binary DNSServRespMsg where
    put (DNSOlleh x) = putWord8 2 >> put x
    put DNSWrongHost = putWord8 6
    get = do
        tag <- getWord8
        if | tag == 2 -> DNSOlleh <$> get
           | tag == 6 -> pure DNSWrongHost
           | otherwise -> fail "Unexpected tag"

instance Binary DNSClientReqMsg where
    put (DNSRequest x) = putWord8 3 >> put x
    get = do
        tag <- getWord8
        if | tag == 3 -> DNSRequest <$> get
           | otherwise -> fail "Unexpected tag"

instance Binary DNSClientRespMsg where
    put (DNSResponseIP x)  = putWord8 4 >> put x
    put DNSResponseUnknown = putWord8 5
    get = do
        tag <- getWord8
        if | tag == 4 -> DNSResponseIP <$> get
           | tag == 5 -> pure DNSResponseUnknown
           | otherwise -> fail "Unexpected tag"

class MonadIO m => MonadDNS m where
    myHost   :: m HostName
    askState :: m (TVar HostMap, TVar HostMap)

    sendMulticast :: ByteString -> m ()
    sendDirectly  :: ByteString -> SockAddr -> m ()
    recvMulticast :: m (ByteString, SockAddr)

    default askState
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => m (TVar HostMap, TVar HostMap)
    askState = lift askState

    default sendMulticast
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => ByteString -> m ()
    sendMulticast = lift . sendMulticast

    default recvMulticast
        :: (MonadTrans t, MonadDNS m', t m' ~ m) => m (ByteString, SockAddr)
    recvMulticast = lift recvMulticast

