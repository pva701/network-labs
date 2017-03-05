{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Serve
       ( serveDNS
       ) where

import           Control.Concurrent.STM (readTVar, writeTVar)
import qualified Data.HashMap.Strict    as HM
import           Network.Socket         (SockAddr)
import           Universum

import           Common                 (MonadThread (..))
import           DNS.Types              (DNSClientReqMsg (..), DNSClientRespMsg (..),
                                         DNSServReqMsg (..), DNSServRespMsg (..),
                                         MonadDNS (..))

serveDNS :: (MonadThread m, MonadDNS m) => m ()
serveDNS = do
    fork dnsListeners
    fork dnsWorkers
    notImplemented -- send request

dnsListeners :: forall m . MonadDNS m => m ()
dnsListeners = do
    --(bytes, _, addr) <- liftIO $ recvFrom sock 1024
    let msg = notImplemented
    notImplemented
  where
    reqListeners :: Either DNSServReqMsg DNSClientReqMsg -> SockAddr -> m ()
    reqListeners (Left (DNSHello host)) from = do
        knownHosts <- atomically . readTVar =<< askHosts
        let msg = DNSOlleh knownHosts
        sendDirectly notImplemented from

    reqListeners (Left (DNSPing host)) from = notImplemented

    reqListeners (Right (DNSRequest host)) from = do
        knownHosts <- atomically . readTVar =<< askHosts
        let msg = maybe DNSResponseUnknown DNSResponseIP (HM.lookup host knownHosts)
        sendDirectly notImplemented from

    respListeners :: DNSServRespMsg -> m ()
    respListeners (DNSOlleh hm) = do
        var <- askHosts
        knownHosts <- atomically $ readTVar var
        atomically $ writeTVar var $ HM.union knownHosts hm

dnsWorkers :: (MonadThread m, MonadDNS m) => m ()
dnsWorkers = do
    ownHost <- myHost
    let msg = DNSPing ownHost
    sendMulticast notImplemented
    delay 50

