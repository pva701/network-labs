{-# LANGUAGE ScopedTypeVariables #-}

module Service.Consumer
       ( runConsumer
       ) where

import           Control.Concurrent.STM               (TVar, modifyTVar, newTVar,
                                                       newTVarIO, readTVar, readTVarIO,
                                                       writeTVar)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromJust)
import qualified Data.Set                             as S
import qualified Data.Text.Lazy                       as TL
import           Network.HTTP.Types.Status            (status200, status404)
import           Network.Multicast                    (multicastReceiver)
import           Network.Socket                       (SockAddr (..), Socket)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Prelude                              (read)
import           Universum                            hiding (ByteString)
import qualified Web.Scotty                           as Sc

import           DNS.Common                           (createUdpSocket, delay, logInfo,
                                                       recvMsg, sendMsg)
import           DNS.Serve                            (runDNS)
import           DNS.Types                            (IPv4, RawAddress, toHostAddress)
import           Service.Common                       (hasFile, toUrl)
import           Service.Types                        (ConsumerMessage (..),
                                                       ConsumerState)

runConsumer :: RawAddress -> RawAddress -> RawAddress -> IO ()
runConsumer (ipv4str, fromIntegral -> port) (ownHost, fromIntegral -> httpPort) dnsAddress = do
    let multicastAddr = SockAddrInet (fromIntegral port) (toHostAddress $ read @IPv4 ipv4str)
    logInfo $ "Consumer mode"
    logInfo $ "Consumer http address: " <> ownHost <> ":" <> show httpPort
    logInfo $ "Consumer multicast address: " <> ipv4str <> ":" <> show port

    knownVar <- runDNS dnsAddress ownHost
    unicastSocket <- createUdpSocket
    multicastSocket <- multicastReceiver ipv4str port

    st <- (fromIntegral httpPort, knownVar,) <$> newTVarIO mempty
    ownIP <- fromJust . M.lookup ownHost <$> readTVarIO knownVar
    void $ forkIO $ runReaderT (consumerListener multicastSocket) st
    void $ forkIO $ consumerWorker unicastSocket multicastAddr ownIP

    application <- Sc.scottyApp $ runReaderT consumerWebApp st
    Warp.run httpPort $ logStdoutDev application

consumerWebApp :: ReaderT ConsumerState Sc.ScottyM ()
consumerWebApp = do
    st@(httpPort, _, _) <- ask
    lift $ do
        Sc.get (Sc.regex "^/download/(.*)$") $ do
            (filename :: FilePath) <- Sc.param "1"
            ipMB <- lift $ runReaderT (handleRequest filename) st
            case ipMB of
                Nothing -> do
                    Sc.status status404
                    Sc.text "File not found"
                Just ip ->
                    Sc.redirect $ TL.pack $ toUrl (ip, httpPort) ("download/" ++ filename)
        Sc.get (Sc.regex "^/file/(.*)$") $ do
            (filename :: FilePath) <- Sc.param "1"
            ipMB <- lift $ runReaderT (handleRequest filename) st
            case ipMB of
                Just _  -> Sc.status status200
                Nothing -> Sc.status status404 >> Sc.text "File not found"
        Sc.notFound $ Sc.status status404 >> Sc.text "Not found"

handleRequest :: String -> ReaderT ConsumerState IO (Maybe IPv4)
handleRequest filename = do
    (port, knownHostsVar, consumersVar) <- ask
    lift $ do
        consumers <- readTVarIO consumersVar
        producers <- toList . (`S.difference` consumers) . S.fromList . toList <$> readTVarIO knownHostsVar
        let l = length producers
        ipVar <- atomically $ newTVar (l, Nothing)
        mapM_ (forkIO . (uncurry $ hasFile' port)) $ zip (repeat ipVar) producers
        atomically $ waitIP ipVar
  where
    waitIP :: TVar (Int, Maybe IPv4)-> STM (Maybe IPv4)
    waitIP ipVar = do
        (cnt, ipMB) <- readTVar ipVar
        case ipMB of
            Just ip -> pure $ Just ip
            Nothing
                | cnt == 0  -> pure Nothing
                | otherwise -> retry

    hasFile' :: Word16 -> TVar (Int, Maybe IPv4) -> IPv4 -> IO ()
    hasFile' port ipVar ipv4 = do
        has <- hasFile (ipv4, port) filename
        let res = if has then Just ipv4 else Nothing
        atomically $ modifyTVar ipVar $ (\(x, y) -> (x - 1, y <|> res))

consumerWorker :: Socket -> SockAddr -> IPv4 -> IO ()
consumerWorker unicastSocket addr ownIP = do
    sendMsg unicastSocket addr $ ConsumerIP ownIP
    delay 1500
    consumerWorker unicastSocket addr ownIP

consumerListener :: Socket -> ReaderT ConsumerState IO ()
consumerListener socket = do
    (_, dnsHostsVar, consumersVar) <- ask
    (ConsumerIP ip, _) <- recvMsg socket
    atomically $ do
        consumers <- S.insert ip <$> readTVar consumersVar
        dnsHosts <- S.fromList . toList <$> readTVar dnsHostsVar
        writeTVar consumersVar (S.intersection consumers dnsHosts)
    consumerListener socket
