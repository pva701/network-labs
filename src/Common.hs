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

instance MonadThread m => MonadThread (ReaderT e m) where
    fork m = lift . fork . runReaderT m =<< ask
    delay = lift .  delay

instance MonadThread IO where
    fork act = () <$ forkIO act
    delay = threadDelay

tryDecode :: Binary a => BSL.ByteString -> Either String a
tryDecode bytes = case decodeOrFail bytes of
    Left (_, _, er) -> Left er
    Right (_, _, v) -> pure v
