{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Serve
       ( runDNS
       ) where

import           Control.Concurrent.STM    (modifyTVar', newTVarIO, readTVar, throwSTM,
                                            writeTVar)
import           Control.Exception         (throwIO)
import           Data.Binary               (decodeOrFail, encode)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Hashable             (hash)
import qualified Data.Map.Strict           as M
import           Network.BSD               (getProtocolNumber)
import           Network.Multicast         (multicastReceiver)
import           Network.Socket            (Family (AF_INET), HostName,
                                            SockAddr (SockAddrInet), Socket,
                                            SocketType (Datagram), socket)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Prelude                   (read)
import           System.Timeout            (timeout)
import           Universum                 hiding (ByteString)

import           DNS.Trans                 (DNSHolder (..), runDNSHolder)
import           DNS.Types                 (DNSException (..), DNSMessage (..),
                                            DNSReq (..), DNSResp (..), DNSState (..),
                                            HostMap, IPv4, fromHostAddress, toHostAddress)

runDNS :: String -> Word16 -> String -> IO ()
runDNS ipv4str (fromIntegral -> port) ownHost = do
    let ipv4 = read @IPv4 ipv4str
    unicastSocket <- createUdpSocket
    multicastSocket <- multicastReceiver ipv4str port
    let multiAddr = SockAddrInet (fromIntegral port) (toHostAddress ipv4)
    varPings <- liftIO $ newTVarIO mempty
    let runner varKnown =
          runDNSHolder varKnown varPings ownHost multicastSocket unicastSocket multiAddr

    varKnownEmpty <- liftIO $ newTVarIO mempty
    known <- runner varKnownEmpty $ joinNetwork
    varKnown <- liftIO $ newTVarIO known
    logInfo $ "Listeners are starting..."
    void $ forkIO $ runner varKnown $ dnsListeners
    logInfo $ "Ping worker is starting..."
    runner varKnown $ dnsPingWorker

joinNetwork :: DNSHolder IO HostMap
joinNetwork = do
    unicastSocket <- asks uSendSocket
    multicastSocket <- asks mRecvSocket
    own <- asks ownHost
    addr <- asks multicastAddress
    let helloRetry = 3
    let joinTimeout = 1000000
    sendMsg unicastSocket addr $ Rq $ DNSHello own
    res <- liftIO $
               rep helloRetry $
                 timeout joinTimeout $
                   joinDo unicastSocket
    case res of
        Nothing -> do
            logInfo "I am alone here"
            (_, ownAddr) <- recvMsg multicastSocket
            let ipMB = addrToIPv4 ownAddr
            case ipMB of
                Nothing -> liftIO $ throwIO UnknownIPFormat
                Just ip -> do
                    logInfo $ "My IP: " ++ show ip
                    pure $ M.singleton own ip
        Just (DNSOlleh myIP known) -> do
            logInfo $ "My IP: " ++ show myIP
            (_, _) <- recvMsg multicastSocket
            pure known
        _             ->
            liftIO $ throwIO SuchHostAlreadyExists
  where
    rep :: Int -> IO (Maybe a) -> IO (Maybe a)
    rep 0 _ = pure Nothing
    rep n act = do
        res <- act
        case res of
            Nothing -> rep (n - 1) act
            _       -> pure res

    joinDo :: Socket -> IO DNSResp
    joinDo unicastSocket = do
        logInfo $ "Trying to join..."
        (msg, _) <- recvMsg unicastSocket
        logInfo $ "Received (during join): " <> show msg
        case msg of
            Rsp ms@(DNSOlleh _ _) -> pure ms
            Rsp ms@DNSWrongHost   -> pure ms
            _                     -> joinDo unicastSocket

dnsListeners :: DNSHolder IO ()
dnsListeners = do
    multicast <- asks mRecvSocket
    (msg, from) <- recvMsg multicast
    logInfo $ "Message: " ++ show msg ++ " has been received, from: " ++ show from
    case msg of
        Rq ms -> messageHandler ms from
        Rsp _ -> logInfo $ "Unexpected message"
    dnsListeners
  where
    messageHandler :: DNSReq -> SockAddr -> DNSHolder IO ()
    messageHandler (DNSHello host) from = whenM ((== Me) <$> whoReply host) $ do
        varKnownHosts <- asks activeHosts
        do (known, response) <- atomically $ do
                knownHosts <- readTVar varKnownHosts
                let ipv4 = addrToIPv4 from
                case ipv4 of
                    Nothing -> throwSTM $ UnknownIPFormat
                    Just ip
                        | M.member host knownHosts -> pure (knownHosts, DNSWrongHost)
                        | otherwise -> do
                            pure (knownHosts, DNSOlleh ip knownHosts)
           logInfo $ "Known hosts: " ++ show known
           sendUnicastMsg from $ Rsp response
        `catch` (\(e::SomeException) -> logInfo $ show e)

    messageHandler (DNSPing host) from = do
        varPingHosts <- asks pingHosts
        case addrToIPv4 from of
            Nothing -> logInfo "Unknown IP format"
            Just ip -> atomically $ modifyTVar' varPingHosts $ M.insert host ip

    messageHandler (DNSResolve host) from = do
        knownHosts <- atomically . readTVar . activeHosts =<< ask
        who <- whoReply host
        when (who == Me) $
            sendUnicastMsg from $ Rsp $ DNSResolved $ M.lookup host knownHosts

dnsPingWorker :: DNSHolder IO ()
dnsPingWorker = do
    myHost <- asks ownHost
    address <- asks multicastAddress
    sendUnicastMsg address $ Rq $ DNSPing myHost
    delay 1000
    varKnown <- asks activeHosts
    varPings <- asks pingHosts
    pings <- atomically $ readTVar varPings
    logInfo $ "Ping hosts: " ++ show pings
    atomically $ do
        readTVar varPings >>= writeTVar varKnown
        writeTVar varPings mempty
    dnsPingWorker

data WhoReply = Me | Other
    deriving Eq

whoReply :: HostName -> DNSHolder IO WhoReply
whoReply host = do
    own <- asks ownHost
    varKnown <- asks activeHosts
    known <- atomically $ readTVar varKnown
    pure $
        if | own == host -> Me
           | host `M.member` known -> Other
           | who known == own -> Me
           | otherwise -> Other
  where
    h = hash host
    who known = fromMaybe "" $
      fmap snd $ head $ sort $
      map (first ((xor h) . hash) . join (,) . fst) $
      M.toList known

-----------------
-- Utilities
-----------------

sendUnicastMsg :: SockAddr -> DNSMessage -> DNSHolder IO ()
sendUnicastMsg addr msg = do
    unicast <- asks uSendSocket
    sendMsg unicast addr msg

sendMsg :: MonadIO m => Socket -> SockAddr -> DNSMessage -> m ()
sendMsg sock addr msg = void $ liftIO $ sendTo sock (BSL.toStrict $ encode msg) addr

recvMsg :: MonadIO m => Socket -> m (DNSMessage, SockAddr)
recvMsg sock = do
    let maxSize = 65536
    (bytes, addr) <- liftIO $ recvFrom sock maxSize
    case decodeOrFail (BSL.fromStrict bytes) of
        Left (_, _, er) -> do
            logInfo $ "Couldn't parse msg: " <> er
            recvMsg sock
        Right (_, _, v) -> pure (v, addr)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . print

delay :: MonadIO m => Int -> m ()
delay = liftIO . threadDelay . (1000 *)

createUdpSocket :: IO Socket
createUdpSocket = socket AF_INET Datagram =<< getProtocolNumber "udp"

addrToIPv4 :: MonadFail m => SockAddr -> m IPv4
addrToIPv4 (SockAddrInet _ ip) = pure $ fromHostAddress ip
addrToIPv4 _                   = fail "Unknown format"
