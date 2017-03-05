module Main where

import           Universum

import           DNS.Trans (runDNSHolder)
import           DNS.Serve (serveDNS)
import           Options   (Args (..), getOptions)

main :: IO ()
main = do
    Args{..} <- getOptions
    runDNSHolder ownHost dnsIP dnsPort serveDNS
