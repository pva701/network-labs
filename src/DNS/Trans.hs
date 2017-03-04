module DNS.Trans
       ( DNSHolder (..)
       ) where

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
    askHosts = DNSHolder $ asks activeHosts
    sendMulticast bytes = DNSHolder $ do
        (sock, addr) <- asks sendSocket
        () <$ liftIO (sendTo sock bytes addr)
    recvMulticast = DNSHolder $ do
        sock <- asks recvSocket
        liftIO $ recvFrom sock 1024 -- TODO length
