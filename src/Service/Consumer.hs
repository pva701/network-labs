{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Service.Consumer
       ( runConsumer
       ) where

import           Control.Concurrent.STM               (TVar, readTVarIO)
import           Control.Monad.Par                    (IVar, Par, get, new, put_,
                                                       runParIO)
import           Data.ByteString.Lazy                 (ByteString)
import           Network.HTTP.Types.Status            (status200, status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Universum                            hiding (ByteString)
import qualified Web.Scotty                           as Sc (ScottyM, get, notFound,
                                                             param, raw, regex, scottyApp,
                                                             status, text)

import           DNS.Serve                            (runDNS)
import           DNS.Types                            (HostMap, IPv4, RawAddress)
import           Service.Common                       (requestFile)

runConsumer :: RawAddress -> RawAddress -> IO ()
runConsumer address (ownHost, fromIntegral -> httpPort) = do
    knownVar <- runDNS address ownHost
    application <- Sc.scottyApp $ consumerWebApp knownVar
    Warp.run httpPort $ logStdoutDev  application

consumerWebApp :: TVar HostMap -> Sc.ScottyM ()
consumerWebApp knownHostsVar = do
    Sc.get (Sc.regex "^/(.*)$") $ do
        (filename :: FilePath) <- Sc.param "1"
        fileMB <- lift $ handleRequest knownHostsVar filename
        case fileMB of
            Nothing -> do
                Sc.status status404
                Sc.text "File not found"
            Just bytes -> do
                Sc.status status200
                Sc.raw bytes
    Sc.notFound $ Sc.status status404 >> Sc.text "Not found"

handleRequest :: TVar HostMap -> String -> IO (Maybe ByteString)
handleRequest knownHostsVar filename = do
    ips <- toList <$> readTVarIO knownHostsVar
    let l = length ips
    ivars <- runParIO $ replicateM l new
    mapM_ (forkIO . (uncurry reqFile)) $ zip ivars ips
    runParIO $ foldM putBS Nothing ivars
  where
    putBS :: Maybe ByteString -> IVar (Maybe ByteString) -> Par (Maybe ByteString)
    putBS r@(Just _) _ = pure r
    putBS _ ivar       = get ivar

    reqFile :: IVar (Maybe ByteString) -> IPv4 -> IO ()
    reqFile ivar ipv4 = runParIO . put_ ivar =<< requestFile ipv4 filename
