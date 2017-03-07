{-# LANGUAGE TypeFamilies #-}

module DNS.Types
       ( DNSState (..)
       , RawAddress

       , DNSReq (..)
       , DNSResp (..)
       , DNSMessage (..)
       , HostMap
       , DNSException (..)

       , HostName
       , IPv4
       , fromHostAddress
       , toHostAddress
       ) where

import           Control.Concurrent.STM (TVar)
import           Data.Binary            (Binary (..), getWord8, putWord8)
import           Data.IP                (IPv4, fromHostAddress, toHostAddress)
import           Network.Socket         (HostName, SockAddr, Socket)
import           Universum              hiding (ByteString)

type HostMap = Map HostName IPv4
type RawAddress = (HostName, Word16)

data DNSState = DNSState
    { activeHosts      :: !(TVar HostMap)
    , pingHosts        :: !(TVar HostMap)
    , ownHost          :: !HostName
    , mRecvSocket      :: !Socket
    , uSendSocket      :: !Socket
    , multicastAddress :: !SockAddr
    }

data DNSReq
    = DNSHello !HostName
    | DNSPing !HostName
    | DNSResolve !HostName
    deriving (Generic, Show)

data DNSResp
    = DNSOlleh !IPv4 !HostMap
    | DNSWrongHost
    | DNSResolved !(Maybe IPv4)
    deriving (Generic, Show)

data DNSMessage = Rq DNSReq | Rsp DNSResp
    deriving Show

data DNSException
    = SuchHostAlreadyExists
    | UnknownIPFormat
    deriving Show
instance Exception DNSException

instance Binary IPv4 where
    put = put . toHostAddress
    get = fromHostAddress <$> get

instance Binary DNSReq where
instance Binary DNSResp where

instance Binary DNSMessage where
    put (Rq x)  = putWord8 0 >> put x
    put (Rsp x) = putWord8 1 >> put x
    get = do
        tag <- getWord8
        if | tag == 0 -> Rq <$> get
           | tag == 1 -> Rsp <$> get
           | otherwise -> fail "Unexpected tag"
