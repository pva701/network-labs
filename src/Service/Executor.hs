{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.Executor
       ( runExecutor
       ) where

import           Control.Concurrent.STM               (TVar, modifyTVar, newTVarIO,
                                                       readTVar, readTVarIO)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time.Clock.POSIX                (POSIXTime, getPOSIXTime)
import           Network.HTTP.Types.Status            (status200, status404)
import           Network.Multicast                    (multicastReceiver)
import           Network.Socket                       (SockAddr (..))
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Prelude                              (read)
import           Universum
import qualified Web.Scotty                           as Sc

import           DNS.Common                           (createUdpSocket, delay, logInfo,
                                                       recvMsg, sendMsg)
import           DNS.Serve                            (runDNS)
import           DNS.Types                            (IPv4, RawAddress, toHostAddress)
import           Service.Common                       (toUrl)
import           Service.Types                        (ExecMessage (..), ExecState (..))

runExecutor :: Int -> RawAddress -> RawAddress -> RawAddress -> IO ()
runExecutor maxLoad (ipv4str, fromIntegral -> port) (ownHost, fromIntegral -> httpPort) dnsAddr  = do
    logInfo $ "Executor mode"
    logInfo $ "Max Load: " <> show maxLoad
    logInfo $ "Exectutor multicast address: " <> ipv4str <> ":" <> show port
    logInfo $ "Exectutor http address: " <> ownHost <> ":" <> show httpPort

    knownVar <- runDNS dnsAddr ownHost
    let ipv4 = read @IPv4 ipv4str
    let multicastAddr = SockAddrInet (fromIntegral port) (toHostAddress ipv4)
    st <- ExecState ownHost (fromIntegral httpPort) <$>
          (fromJust . M.lookup ownHost <$> readTVarIO knownVar) <*>
          newTVarIO maxLoad <*>
          newTVarIO mempty <*>
          createUdpSocket <*>
          multicastReceiver ipv4str port <*>
          pure multicastAddr

    void $ forkIO $ runReaderT executorListener st
    void $ forkIO $ runReaderT executorWorker st
    application <- Sc.scottyApp $ runReaderT executorWebApp st
    Warp.run httpPort $ logStdoutDev  application

executorWebApp :: ReaderT ExecState Sc.ScottyM ()
executorWebApp = do
    st@ExecState {..} <- ask
    lift $ do
        Sc.get "/fib/:n" $ do
            (n :: Int) <- Sc.param "n"
            resMB <- liftIO $ runReaderT (tryExecWithTime (pure $ fibonacciTask n 0 1)) st
            case resMB of
                Just (res, tm) -> do
                    Sc.text $ TL.fromStrict $ T.intercalate "\n" $
                      [ "Executor: " <> (T.pack ownHttpHost) <> "/" <> (show ownIP)
                      , "N: " <> show n
                      , "Result: " <> show res
                      , "Time: " <> show tm]
                    Sc.status status200
                Nothing -> runReaderT (searchRedirect $ "/fib/" ++ show n) st
        Sc.get "/sleep/:time" $ do
            (slTime :: Int) <- Sc.param "time"
            resMB <- liftIO $ runReaderT (tryExecWithTime $ sleepTask slTime) st
            case resMB of
                Just (_, tm) -> do
                    Sc.text $ TL.fromStrict $ T.intercalate "\n" $
                      [ "Executor: " <> (T.pack ownHttpHost) <> "/" <> (show ownIP)
                      , "Seconds: " <> show slTime
                      , "Time: " <> show tm]
                    Sc.status status200
                Nothing -> runReaderT (searchRedirect $ "/sleep/" ++ show slTime) st

searchRedirect :: FilePath -> ReaderT ExecState Sc.ActionM ()
searchRedirect redPath = do
    ExecState {..} <- ask
    lift $ do
        othersFree <- atomically $ readTVar othersFreeVar
        let freeMB = find (\x -> snd x > 0 && fst x /= ownIP) (M.toList othersFree)
        case freeMB of
            Nothing        ->
                Sc.status status404 >> Sc.text "Free thread not found"
            Just (ipv4, _) ->
                Sc.redirect $ TL.pack $ toUrl (ipv4, httpPort) redPath

tryExecWithTime :: IO a -> ReaderT ExecState IO (Maybe (a, POSIXTime))
tryExecWithTime task = do
    st@ExecState {..} <- ask
    lift $ do
        startTime <- liftIO $ getPOSIXTime
        resMB <- liftIO $ runReaderT (tryExecTask task) st
        endTime <- liftIO $ getPOSIXTime
        case resMB of
            Just res -> pure $ Just (res, endTime - startTime)
            Nothing  -> pure Nothing

tryExecTask :: IO a -> ReaderT ExecState IO (Maybe a)
tryExecTask task = do
    ExecState{..} <- ask
    lift $ do
        numFreeMB <- tryAcquire freeVar
        case numFreeMB of
            Nothing -> pure Nothing
            Just free -> do
                sendMsg unicastSocket multicastAddress $ FreeThreads ownIP free
                res <- task
                sendMsg unicastSocket multicastAddress . FreeThreads ownIP =<< release freeVar
                pure $ Just res
  where
    tryAcquire :: TVar Int -> IO (Maybe Int)
    tryAcquire freeVar = atomically $ do
        busy <- readTVar freeVar
        if | busy == 0 -> pure Nothing
           | otherwise -> do
               modifyTVar freeVar (\x->x-1)
               Just <$> readTVar freeVar

    release :: TVar Int -> IO Int
    release freeVar = atomically $ do
        modifyTVar freeVar (+1)
        readTVar freeVar

-- f0 = 1, f1 = 1, f2 = 2,...
fibonacciTask :: Int -> Integer -> Integer -> Integer
fibonacciTask !n !a !b
    | n == 0    = b
    | otherwise = fibonacciTask (n - 1) b (a + b)

sleepTask :: Int -> IO ()
sleepTask = delay . (*1000)

executorWorker :: ReaderT ExecState IO ()
executorWorker = do
    ExecState {..} <- ask
    free <- atomically $ readTVar freeVar
    liftIO $ sendMsg unicastSocket multicastAddress $ FreeThreads ownIP free
    delay 1000

executorListener :: ReaderT ExecState IO ()
executorListener = do
    ExecState {..} <- ask
    (FreeThreads ip free, _) <- recvMsg multicastSocket
    atomically $ modifyTVar othersFreeVar (M.insert ip free)
    executorListener
