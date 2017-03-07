-- | Misha is vegan

module DNS.Resolve
       ( resolveHost
       ) where

import           Data.IP        (IPv4)
import           Network.Socket (HostName, SockAddr (SockAddrInet))
import           Prelude        (read)
import           System.Timeout (timeout)
import           Universum

import           DNS.Common     (createUdpSocket, recvMsg, repeatN, sendMsg)
import           DNS.Types      (DNSMessage (..), DNSReq (DNSResolve),
                                 DNSResp (DNSResolved), RawAddress, toHostAddress)

resolveHost :: HostName -> RawAddress -> IO (Maybe IPv4)
resolveHost host (ipv4str, port) = do
    let ipv4 = read @IPv4 ipv4str
    let multiAddr = SockAddrInet (fromIntegral port) (toHostAddress ipv4)

    let resolveRetry = 3
    let resolveTimeout = 1000000
    unicastSocket <- createUdpSocket
    res <- repeatN resolveRetry $
      timeout resolveTimeout $
        resolveDo multiAddr unicastSocket
    pure $
        case res of
            Nothing      -> Nothing
            Just Nothing -> Nothing
            Just j       -> j
  where
    resolveDo addr unicastSocket = do
        sendMsg unicastSocket addr $ Rq $ DNSResolve host
        (msg, _) <- recvMsg unicastSocket
        pure $
            case msg of
                Rsp (DNSResolved j@(Just _)) -> j
                _                            -> Nothing
