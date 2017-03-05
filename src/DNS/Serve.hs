{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Serve
       ( serveDNS
       ) where

import           Control.Concurrent.STM (modifyTVar', readTVar, writeTVar)
import           Data.Binary            (encode)
import qualified Data.Map.Strict        as M
import           Network.Socket         (SockAddr (..), hostAddressToTuple)
import           Universum              hiding (ByteString)

import           Common                 (MonadThread (..), tryDecode)
import           DNS.Types              (DNSClientReqMsg (..), DNSClientRespMsg (..),
                                         DNSServReqMsg (..), DNSServRespMsg (..), IPv4,
                                         MonadDNS (..))

serveDNS :: (MonadThread m, MonadDNS m) => m ()
serveDNS = do
    fork dnsListeners
    own <- myHost
    sendMulticast $ encode $ DNSHello own
    dnsPingWorker

dnsListeners :: forall m . MonadDNS m => m ()
dnsListeners = do
    (bytes, from) <- recvMulticast
    let res1 = tryDecode @DNSServReqMsg bytes
    let res2 = tryDecode @DNSClientReqMsg bytes
    let res3 = tryDecode @DNSServRespMsg bytes
    whenRight res1 $ flip reqServListeners from
    whenRight res2 $ flip reqCliListeners from
    whenRight res3 respServListeners
    --when (isLeft res1 && isLeft res2 && isLeft res3) $ notImplemented
  where
    addrToIPv4 :: SockAddr -> IPv4
    addrToIPv4 (SockAddrInet _ ip) = hostAddressToTuple ip
    addrToIPv4 _                   = notImplemented

    reqServListeners :: DNSServReqMsg -> SockAddr -> m ()
    reqServListeners (DNSHello host) from = do
        var <- fst <$> askState
        msg <- atomically $ do
            knownHosts <- readTVar var
            let ipv4 = addrToIPv4 from
            if | M.member host knownHosts -> pure DNSWrongHost
               | otherwise -> do
                   let res = M.insert host ipv4 knownHosts
                   DNSOlleh res <$ writeTVar var res
        sendDirectly (encode msg) from
    reqServListeners (DNSPing host) from = do
        var <- snd <$> askState
        atomically $ modifyTVar' var $ M.insert host (addrToIPv4 from)

    reqCliListeners :: DNSClientReqMsg -> SockAddr -> m ()
    reqCliListeners (DNSRequest host) from = do
        knownHosts <- atomically . readTVar . fst =<< askState
        let msg = maybe DNSResponseUnknown DNSResponseIP (M.lookup host knownHosts)
        sendDirectly (encode msg) from

    respServListeners :: DNSServRespMsg -> m ()
    respServListeners (DNSOlleh newHosts) = do
        var <- fst <$> askState
        knownHosts <- atomically $ readTVar var
        atomically $ writeTVar var $ M.union knownHosts newHosts
    respServListeners DNSWrongHost = notImplemented

dnsPingWorker :: (MonadThread m, MonadDNS m) => m ()
dnsPingWorker = forever $ do
    ownHost <- myHost
    sendMulticast $ encode $ DNSPing ownHost
    delay 50
    (knownVar, pingsVar) <- askState
    atomically $ readTVar pingsVar >>= writeTVar knownVar
