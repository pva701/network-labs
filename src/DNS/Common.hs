module DNS.Common
       ( logInfo
       , delay
       , createUdpSocket
       , repeatN
       , sendUnicastMsg
       , sendMsg
       , recvMsg
       , addrToIPv4
       ) where

import           Data.Binary               (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy      as BSL
import           Network.BSD               (getProtocolNumber)
import           Network.Socket            (Family (AF_INET), SockAddr (SockAddrInet),
                                            Socket, SocketType (Datagram), socket)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Universum

import           DNS.Trans                 (DNSHolder)
import           DNS.Types                 (DNSMessage, IPv4, fromHostAddress,
                                            uSendSocket)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn

delay :: MonadIO m => Int -> m ()
delay = liftIO . threadDelay . (1000 *)

createUdpSocket :: IO Socket
createUdpSocket = socket AF_INET Datagram =<< getProtocolNumber "udp"

repeatN :: Int -> IO (Maybe a) -> IO (Maybe a)
repeatN 0 _ = pure Nothing
repeatN n act = do
    res <- act
    case res of
        Nothing -> repeatN (n - 1) act
        _       -> pure res

sendUnicastMsg :: SockAddr -> DNSMessage -> DNSHolder IO ()
sendUnicastMsg addr msg = do
    unicast <- asks uSendSocket
    sendMsg unicast addr msg

sendMsg :: (Binary a, MonadIO m) => Socket -> SockAddr -> a -> m ()
sendMsg sock addr msg = void $ liftIO $ sendTo sock (BSL.toStrict $ encode msg) addr

recvMsg :: (Binary a, MonadIO m) => Socket -> m (a, SockAddr)
recvMsg sock = do
    let maxSize = 65536
    (bytes, addr) <- liftIO $ recvFrom sock maxSize
    case decodeOrFail (BSL.fromStrict bytes) of
        Left (_, _, er) -> do
            logInfo $ "Couldn't parse msg: " <> er
            recvMsg sock
        Right (_, _, v) -> pure (v, addr)


addrToIPv4 :: MonadFail m => SockAddr -> m IPv4
addrToIPv4 (SockAddrInet _ ip) = pure $ fromHostAddress ip
addrToIPv4 _                   = fail "Unknown format"
