module DNS.Trans
       ( DNSHolder (..)
       , runDNSHolder
       ) where

import           Control.Concurrent.STM    (newTVar)
import           Data.ByteString.Lazy      as BSL
import           Network.Multicast         (multicastReceiver, multicastSender)
import           Network.Socket            (HostName)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Universum

import           Common                    (MonadThread)
import           DNS.Types                 (DNSState (..), MonadDNS (..))

newtype DNSHolder m a = DNSHolder
    { getDNSHolder :: ReaderT DNSState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThread
               )

instance MonadIO m => MonadDNS (DNSHolder m) where
    myHost = DNSHolder $ asks ownHost
    askState = DNSHolder $ asks (bimap activeHosts pings . join (,))
    sendMulticast (BSL.toStrict -> bytes) = DNSHolder $ do
        (sock, addr) <- asks sendSocket
        () <$ liftIO (sendTo sock bytes addr)
    sendDirectly (BSL.toStrict -> bytes) addr = DNSHolder $ do
        (sock, _) <- asks sendSocket
        () <$ liftIO (sendTo sock bytes addr)
    recvMulticast = DNSHolder $ do
        sock <- asks recvSocket
        liftIO (first BSL.fromStrict <$> recvFrom sock 1024) -- TODO length

runDNSHolder :: MonadIO m => HostName -> String -> Word16 -> DNSHolder m a -> m a
runDNSHolder host ip (fromIntegral -> port) holder = do
    hosts1 <- atomically $ newTVar mempty
    hosts2 <- atomically $ newTVar mempty
    sender <- liftIO $ multicastSender ip port
    recv <- liftIO $ multicastReceiver ip port
    runReaderT (getDNSHolder holder) $ DNSState hosts1 hosts2 host sender recv
