{-# LANGUAGE ApplicativeDo #-}

module Options
       ( Args (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             simpleOptions, strOption, value)
import           Universum

data Args = Args
    { file     :: !String
    , httpHost :: !String
    , httpPort :: !Word16
    , dnsIP    :: !String
    , dnsPort  :: !Word16
    } deriving Show

argsParser :: Parser Args
argsParser = do
    file <- strOption $
        long    "file" <>
        metavar "FILENAME" <>
        help    "File name"
    httpHost <- strOption $
        long "host" <>
        metavar "STRING" <>
        help "Host of node"
    httpPort <- option auto $
        long    "port" <>
        metavar "INT" <>
        value   8080 <>
        help    "Http port"
    dnsIP <- strOption $
        long    "dns-ip" <>
        metavar "IP" <>
        value   "224.0.0.99" <>
        help    "IP on which mDNS will work"
    dnsPort <- option auto $
        long    "dns-port" <>
        metavar "INT" <>
        value   5555 <>
        help    "Port on which  mDNS will work"
    pure Args{..}

getOptions :: IO Args
getOptions = do
    (res, ()) <-
        simpleOptions
            "network-labs-server"
            "Multicast DNS node"
            "Multicast DNS node"
            argsParser
            empty
    pure res
