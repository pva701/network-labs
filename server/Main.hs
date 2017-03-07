module Main where

import           Universum

import           DNS.Serve (runDNS)
import           Options   (Args (..), getOptions)

main :: IO ()
main = do
    Args{..} <- getOptions
    runDNS dnsIP dnsPort ownHost
