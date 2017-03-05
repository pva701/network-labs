{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Serve
       ( serveDNS
       ) where

import           Universum

import           Common    (MonadThread (..))
import           DNS.Types (DNSClientReqMsg (..), DNSClientRespMsg (..),
                            DNSServReqMsg (..), DNSServRespMsg (..), MonadDNS (..))

serveDNS :: (MonadThread m, MonadDNS m) => m ()
serveDNS = do
    fork dnsListeners
    fork dnsWorkers
    notImplemented -- send request

dnsListeners :: MonadDNS m => m ()
dnsListeners = do
    --(bytes, _, addr) <- liftIO $ recvFrom sock 1024
    let msg = notImplemented
    notImplemented
  where
    reqListeners :: Either DNSServReqMsg DNSClientReqMsg -> m ()
    reqListeners (Left (DNSHello host))    = notImplemented
    reqListeners (Left (DNSPing host))     = notImplemented
    reqListeners (Right (DNSRequest host)) = notImplemented

    respListeners :: DNSServRespMsg -> m ()
    respListeners (DNSOlleh hm) = notImplemented

dnsWorkers :: (MonadThread m, MonadDNS m) => m ()
dnsWorkers = do
    sendMulticast notImplemented
    delay 50

