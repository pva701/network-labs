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
        ProducerMode root -> runProducer root httpAddr dnsAddr
        ConsumerMode addr -> runConsumer addr httpAddr dnsAddr
        ExecutorMode maxLoad addr -> runExecutor maxLoad addr httpAddr dnsAddr
