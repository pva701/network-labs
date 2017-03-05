{-# LANGUAGE TypeFamilies #-}

module Common
       ( MonadThread (..)
       , tryDecode
       ) where

import           Control.Monad.Trans  (MonadTrans (lift))
import           Data.Binary          (Binary, decodeOrFail)
import qualified Data.ByteString.Lazy as BSL
import           Universum

class Monad m => MonadThread m where
    fork :: m () -> m ()
    delay :: Int -> m ()

    -- default fork
    --     :: (MonadTrans t, MonadThread m', t m' ~ m) => m () -> m ()
    -- fork = lift . fork

    default delay
        :: (MonadTrans t, MonadThread m', t m' ~ m) => Int -> m ()
    delay = lift . delay

tryDecode :: Binary a => BSL.ByteString -> Either String a
tryDecode bytes = case decodeOrFail bytes of
    Left (_, _, er) -> Left er
    Right (_, _, v) -> pure v
