module Main where

import           Data.ByteString.Lazy (writeFile)
import           Prelude              (putStrLn)
import           Universum            hiding (ByteString, putStrLn, writeFile)

import           DNS.Resolve          (resolveHost)
import           Options              (Action (..), Args (..), getOptions)
import           Service.Common       (requestFile, requestTask)

main :: IO ()
main = do
    Args{..} <- getOptions
    ipMB <- resolveHost httpHost (dnsIP, dnsPort)
    case ipMB of
        Nothing -> putStrLn "Couldn't resolve host"
        Just ip -> case action of
            Download file -> do
                bytesMB <- requestFile (ip, httpPort) file
                case bytesMB of
                    Nothing    -> putStrLn "No such file"
                    Just bytes -> writeFile file bytes
            Exec task arg -> do
                res <- requestTask (ip, httpPort) task arg
                case res of
                    Nothing -> putStrLn "No such task"
                    Just t  -> putText t
