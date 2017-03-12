{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Service.Producer
       ( runProducer
       ) where

import           Network.HTTP.Types.Status            (status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.FilePath                      ((</>))
import           Universum
import qualified Web.Scotty                           as Sc

import           DNS.Common                           (logInfo)
import           DNS.Serve                            (runDNS)
import           DNS.Types                            (RawAddress)


runProducer :: FilePath -> RawAddress -> RawAddress -> IO ()
runProducer rootDir (ownHost, fromIntegral -> httpPort) address  = do
    logInfo $ "Producer mode"
    logInfo $ "Producer http address: " <> ownHost <> ":" <> show httpPort

    void $ runDNS address ownHost
    application <- Sc.scottyApp $ producerWebApp rootDir
    Warp.run httpPort $ logStdoutDev  application

producerWebApp :: FilePath -> Sc.ScottyM ()
producerWebApp rootDir = do
    Sc.get (Sc.regex "^/(.*)$") $ do
        (filename :: FilePath) <- Sc.param "1"
        Sc.file (rootDir </> filename)
    Sc.notFound $ Sc.status status404 >> Sc.text "Not found"
