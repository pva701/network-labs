module Main where

import           Universum

import           DNS.Serve      (runDNS)
import           Options        (Args (..), getOptions)

import           Network.Socket (Family (AF_INET, AF_INET6), isSupportedFamily)

main :: IO ()
main = do
    print $ AF_INET < AF_INET6
    print $ isSupportedFamily AF_INET6
    --Args{..} <- getOptions
    --runDNS dnsIP dnsPort ownHost
