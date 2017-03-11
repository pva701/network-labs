module Main where

import           Data.ByteString.Lazy (writeFile)
import           Prelude              (putStrLn)
import           Universum            hiding (ByteString, putStrLn, writeFile)

import           DNS.Resolve          (resolveHost)
import           Options              (Args (..), getOptions)
import           Service.Common       (requestFile)

main :: IO ()
main = do
    Args{..} <- getOptions
    ipMB <- resolveHost httpHost (dnsIP, dnsPort)
    case ipMB of
        Nothing -> putStrLn "Couldn't resolve host"
        Just ip -> do
            bytesMB <- requestFile ip file
            case bytesMB of
                Nothing    -> putStrLn "No such file"
                Just bytes -> writeFile file bytes
