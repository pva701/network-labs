{-# LANGUAGE ScopedTypeVariables #-}

module Service.Consumer
       ( runConsumer
       ) where

import           Control.Concurrent.STM               (TVar, modifyTVar, newTVar,
                                                       readTVar, readTVarIO)
import qualified Data.Text.Lazy                       as TL
import           Network.HTTP.Types.Status            (status200, status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum                            hiding (ByteString)
import qualified Web.Scotty                           as Sc

import           DNS.Common                           (logInfo)
import           DNS.Serve                            (runDNS)
import           DNS.Types                            (HostMap, IPv4, RawAddress)
import           Service.Common                       (hasFile, toUrl)

runConsumer :: RawAddress -> RawAddress -> IO ()
runConsumer (ownHost, fromIntegral -> httpPort) dnsAddress = do
    logInfo $ "Consumer mode"
    logInfo $ "Consumer http address: " <> ownHost <> ":" <> show httpPort

    knownVar <- runDNS dnsAddress ownHost
    application <- Sc.scottyApp $ runReaderT consumerWebApp (fromIntegral httpPort, knownVar)
    Warp.run httpPort $ logStdoutDev  application

consumerWebApp :: ReaderT (Word16, TVar HostMap) Sc.ScottyM ()
consumerWebApp = do
    st@(httpPort, _) <- ask
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

handleRequest :: String -> ReaderT (Word16, TVar HostMap) IO (Maybe IPv4)
handleRequest filename = do
    (port, knownHostsVar) <- ask
    lift $ do
        ips <- toList <$> readTVarIO knownHostsVar
        let l = length ips
        ipVar <- atomically $ newTVar (l, Nothing)
        mapM_ (forkIO . (uncurry $ hasFile' port)) $ zip (repeat ipVar) ips
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
