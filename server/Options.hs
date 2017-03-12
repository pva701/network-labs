{-# LANGUAGE ApplicativeDo #-}

module Options
       ( Args (..)
       , WorkMode (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (CommandFields, Mod, Parser, ReadM, auto,
                                             command, eitherReader, help, info, long,
                                             metavar, option, progDesc, simpleOptions,
                                             strOption, subparser, value)
import           Prelude                    (read)
import           Text.Parsec                (Parsec, parse)
import qualified Text.Parsec.Char           as P
import qualified Text.Parsec.String         as P
import           Universum

import           DNS.Types                  (RawAddress)

data WorkMode
    = ProducerMode !FilePath
    | ConsumerMode !RawAddress
    | ExecutorMode !Int !RawAddress
    deriving Show

data Args = Args
    { workMode :: !WorkMode
    , httpAddr :: !RawAddress
    , dnsAddr  :: !RawAddress
    } deriving Show

fromParsec :: Parsec String () a -> ReadM a
fromParsec parser = eitherReader $ either (Left . show) Right . parse parser "<CLI options>"

rawAddrParser :: P.Parser RawAddress
rawAddrParser = (,) <$>
             (some $ P.noneOf " :") <* (P.char ':') <*>
             (read <$> some P.digit)

rawAddrOption :: String -> RawAddress -> String -> Parser RawAddress
rawAddrOption name def h =
    option (fromParsec rawAddrParser) $
        long name  <>
        metavar "HOST:PORT" <>
        value def <>
        help h

producerCommand :: Mod CommandFields WorkMode
producerCommand = command "producer" $ info opts desc
  where
    opts = ProducerMode <$> strOption (
            long    "root" <>
            metavar "FILEPATH" <>
            help    "File path to root")
    desc = progDesc "Producer mode"

consumerCommand :: Mod CommandFields WorkMode
consumerCommand = command "consumer" $ info opts desc
  where
    opts = ConsumerMode <$>
           (rawAddrOption "multicast" ("224.0.0.100", 5556) "Consumer multicast address")
    desc = progDesc "Consumer mode"

executorCommand :: Mod CommandFields WorkMode
executorCommand = command "executor" $ info opts desc
  where
    opts = ExecutorMode <$>
           option auto (
               long "max-load" <>
               metavar "INT" <>
               help "Max load of executor") <*>
           (rawAddrOption "multicast" ("224.0.0.101", 5557) "Executor multicast address")
    desc = progDesc "Executor mode"

workModeParser :: Parser WorkMode
workModeParser = subparser $ producerCommand <> consumerCommand <> executorCommand

argsParser :: Parser Args
argsParser = do
    workMode <- workModeParser
    httpAddr <- rawAddrOption "http" ("defaulthost", 8080) "HTTP address of node"
    dnsAddr <- rawAddrOption "dns" ("224.0.0.99", 5555) "DNS multicast address of node"
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
