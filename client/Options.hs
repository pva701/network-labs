{-# LANGUAGE ApplicativeDo #-}

module Options
       ( Args (..)
       , Action (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (CommandFields, Mod, Parser, auto, command,
                                             help, info, long, metavar, option, progDesc,
                                             simpleOptions, strOption, subparser, value)
import           Universum

data Action
    = Download !String
    | Exec !String !Int
    deriving Show


data Args = Args
    { action   :: !Action
    , httpHost :: !String
    , httpPort :: !Word16
    , dnsIP    :: !String
    , dnsPort  :: !Word16
    } deriving Show

downParser :: Mod CommandFields Action
downParser = command "download" $ info opts desc
  where
    opts = do
        Download <$> strOption (
            long    "file" <>
            metavar "FILENAME" <>
            help    "File name")
    desc = progDesc "Download file"

execParser :: Mod CommandFields Action
execParser = command "exec" $ info opts desc
  where
    opts = Exec <$>
            strOption (
                long    "task" <>
                metavar "STRING" <>
                help    "Task name") <*>
            option auto (
                long "arg" <>
                metavar "INT" <>
                help "Task argument"
            )
    desc = progDesc "Execute task"

actionParser :: Parser Action
actionParser = subparser $ downParser <> execParser

argsParser :: Parser Args
argsParser = do
    action <- actionParser
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
