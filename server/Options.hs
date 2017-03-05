{-# LANGUAGE ApplicativeDo #-}

module Options
       ( Args (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             simpleOptions, strOption, value)
import           Universum

data WorkMode = ProducerMode | ConsumerMode
    deriving Show

data Args = Args
    { ownHost :: !String
    , dnsIP   :: !String
    , dnsPort :: !Word16
    } deriving Show

argsParser :: Parser Args
argsParser = do
    -- workMode <- strOption $
    --     long    "work-mode" <>
    --     metavar "SWITCH" <>
    --     help    "WorkMode of node: Consumer or Producer"
    ownHost <- strOption $
        long "host" <>
        metavar "STRING" <>
        help "Host of node in DNS"
    dnsIP <- strOption $
        long    "dns-ip" <>
        metavar "IP" <>
        value   "224.0.0.99" <>
        help    "IP on which mDNS will work"
    dnsPort <- option auto $
        long    "dns-port" <>
        metavar "INT" <>
        value   5353 <>
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
