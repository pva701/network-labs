module Main where

import           Universum

import           Options          (Args (..), WorkMode (..), getOptions)
import           Service.Consumer (runConsumer)

main :: IO ()
main = do
    Args{..} <- getOptions
    case workMode of
        ProducerMode -> notImplemented
        ConsumerMode -> runConsumer (dnsIP, dnsPort) (httpHost, httpPort)
