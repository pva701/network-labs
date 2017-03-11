module Main where

import           Universum

import           Options          (Args (..), WorkMode (..), getOptions)
import           Service.Consumer (runConsumer)
import           Service.Producer (runProducer)

main :: IO ()
main = do
    Args{..} <- getOptions
    case workMode of
        ProducerMode root -> runProducer root (dnsIP, dnsPort) (httpHost, httpPort)
        ConsumerMode      -> runConsumer (dnsIP, dnsPort) (httpHost, httpPort)
