{-# LANGUAGE BangPatterns #-}

module DNS.Trans
       ( DNSHolder (..)
       , runDNSHolder
       ) where

import           Control.Concurrent.STM (TVar)
import           Network.Socket         (HostName)
import           Network.Socket         (SockAddr, Socket)
import           Universum

import           DNS.Types              (DNSState (..), HostMap)

newtype DNSHolder m a = DNSHolder
    { getDNSHolder :: ReaderT DNSState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader DNSState
               , MonadThrow
               , MonadCatch
               )

runDNSHolder :: MonadIO m
             => TVar HostMap
             -> TVar HostMap
             -> HostName
             -> Socket
             -> Socket
             -> SockAddr
             -> DNSHolder m a
             -> m a
runDNSHolder varKnown varPings own r s multiAddr holder = do
    runReaderT (getDNSHolder holder) $ DNSState varKnown varPings own r s multiAddr
