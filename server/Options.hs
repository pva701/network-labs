{-# LANGUAGE ApplicativeDo #-}

module Options
       ( Args (..)
       , WorkMode (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (Parser, ReadM, auto, eitherReader, help,
                                             long, metavar, option, simpleOptions,
                                             strOption, value)
import           Prelude                    (read)
import           Text.Parsec                (Parsec, parse)
import qualified Text.Parsec.Char           as P
import qualified Text.Parsec.String         as P
import           Universum

import           DNS.Types                  (HostName)

data WorkMode
    = ProducerMode !FilePath
    | ConsumerMode
    | ExecutorMode !Int !(HostName, Word16)
    deriving Show

data Args = Args
    { workMode :: !WorkMode
    , httpHost :: !String
    , httpPort :: !Word16
    , dnsIP    :: !String
    , dnsPort  :: !Word16
    } deriving Show

fromParsec :: Parsec String () a -> ReadM a
fromParsec parser = eitherReader $ either (Left . show) Right . parse parser "<CLI options>"

addrParser :: P.Parser (HostName, Word16)
addrParser = (,) <$>
             (some $ P.noneOf " :") <* (P.char ':') <*>
             (read <$> some P.digit)

workModeParser :: P.Parser WorkMode
workModeParser = ProducerMode <$ (P.string "producer") <* (P.char ':') <*> (some $ P.noneOf " ") <|>
                 ConsumerMode <$ (P.string "consumer") <|>
                 ExecutorMode <$ (P.string "executor") <* (P.char ':') <*>
                                 (read <$> some P.digit) <* (P.char ':') <*>
                                 addrParser

argsParser :: Parser Args
argsParser = do
    workMode <- option (fromParsec workModeParser) $
        long    "work-mode" <>
        metavar "MODE" <>
        help    "WorkMode of node: Consumer or Producer"
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
