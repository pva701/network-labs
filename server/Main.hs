module Main where

import           Universum

import           Service.Consumer (runConsumer)
import           Service.Executor (runExecutor)
import           Service.Producer (runProducer)

import           Options          (Args (..), WorkMode (..), getOptions)

main :: IO ()
main = do
    Args{..} <- getOptions
    case workMode of
        ProducerMode root -> runProducer root (httpHost, httpPort) (dnsIP, dnsPort)
        ConsumerMode      -> runConsumer (httpHost, httpPort) (dnsIP, dnsPort)
        ExecutorMode maxLoad addr ->
            runExecutor maxLoad addr (httpHost, httpPort) (dnsIP, dnsPort)
