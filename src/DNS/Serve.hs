{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Serve
       ( serveDNS
       ) where

import           Universum

import           DNS.Types (DNSClientReqMsg (..), DNSClientRespMsg (..),
                            DNSServReqMsg (..), DNSServRespMsg (..), MonadDNS (..))

serveDNS :: MonadDNS m => m ()
serveDNS = do
    -- liftIO $ forkIO dnsListeners
    -- liftIO $ forkIO dnsWorkers
    notImplemented -- send request

dnsListeners :: MonadDNS m => m ()
dnsListeners = do
    --(bytes, _, addr) <- liftIO $ recvFrom sock 1024
    let msg = notImplemented
    notImplemented
  where
    reqListeners :: Either DNSServReqMsg DNSClientReqMsg -> m ()
    reqListeners (Left (DNSHello host)) = notImplemented
    reqListeners (Left (DNSPing host)) = notImplemented
    reqListeners (Right (DNSRequest host)) = notImplemented

    respListeners :: DNSServRespMsg -> m ()
    respListeners (DNSOlleh hm) = notImplemented

dnsWorkers :: MonadDNS m => m ()
dnsWorkers = do
    sendMulticast notImplemented
    notImplemented --sleep
    --sendTo sock "Hello, world" addr

