{-# LANGUAGE TypeFamilies #-}

module Common
       ( MonadThread (..)
       ) where

import           Control.Monad.Trans (MonadTrans (lift))
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
