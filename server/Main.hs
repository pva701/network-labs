module Main where

import           Data.Binary (encode)
import           DNS.Types   (DNSClientReqMsg (..), DNSServReqMsg (..))
import           Universum

main :: IO ()
main = do
    let sv1 = DNSHello "123"
    let sv2 = DNSPing "123"
    let cl = DNSRequest "123"
    print $ encode sv1
    print $ encode sv2
    print $ encode cl
