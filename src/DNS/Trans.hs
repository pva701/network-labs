module DNS.Trans
       ( DNSHolder (..)
       ) where

import           Data.ByteString.Lazy      as BSL
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Universum

import           DNS.Types                 (DNSState (..), MonadDNS (..))

newtype DNSHolder m a = DNSHolder
    { getDNSHolder :: ReaderT DNSState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
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
