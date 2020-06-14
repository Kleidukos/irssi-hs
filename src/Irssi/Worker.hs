{-|
Module      : Irssi.Worker
Description : Background worker that relays Command commands to irssi
Copyright   : © Hécate
License     : MIT
Maintainer  : hecate@glitchbra.in

This module provides a worker that will be spawned in the background.
Communication with this worker is made through a pair of channels
(provided by the [unagi-chan](hackage.haskell.org/package/unagi-chan) library).
-}

module Irssi.Worker
  ( getChatnets
  , getMethods
  , getVersion
  , sendMessage
  , startWorker
  ) where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Chan.Unagi
import           Control.Monad.Except
import           Data.Aeson                    (eitherDecodeStrict, encode)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Network.Socket
import           Network.Socket.ByteString

import           Irssi.Types

-- | Start a background worker that will keep the connection to
--   the UNIX-domain socket.
--   This function returns a @writer@ chan, in which Command command
--   can be sent, and a ''reader' chan, from which the responses
--   will be received.
--
--   === __Example:__
--   > startWorker "/home/user/.irssi.sock"
startWorker :: (MonadIO m, MonadError APIError m) => FilePath -> m IrssiState
startWorker path = do
  (workerWriter, mainReader) <- liftIO newChan
  (mainWriter, workerReader) <- liftIO newChan
  s <- liftIO $ socket AF_UNIX Stream defaultProtocol
  liftIO $ connect s (SockAddrUnix path)
  liftIO $ forkIO $ process (workerWriter, workerReader) s
  makeState (Stargate (mainWriter, mainReader)) path

process :: (InChan ByteString, OutChan Command) -> Socket -> IO ()
process (inChan, outChan) s = do
  command <- readChan outChan
  let payload = toStrict (encode command) <> "\n"
  send s payload
  received <- recv s 8192
  writeChan inChan received
  process (inChan, outChan) s

-- | Send a command to irssi.
--
-- === __Example:__
--
-- > let msg = MkMessage Message{cmd="msg", network="freenode", channel="#bottest", message="I am alive!"}
-- > sendMessage msg (writerChan, readerChan)
sendMessage :: (MonadIO m) => Command -> Stargate -> m ByteString
sendMessage command stargate = do
  let (writerChan, readerChan) = getChans stargate
  liftIO $ writeChan writerChan command
  liftIO $ readChan readerChan

-- | Query the irssi instance to construct a statte
makeState :: (MonadIO m, MonadError APIError m) => Stargate -> FilePath -> m IrssiState
makeState stargate path =
  IrssiState <$> getMethods stargate
             <*> getChatnets stargate
             <*> getVersion stargate
             <*> pure path
             <*> pure stargate

-- | Query the irssi instance to fetch the configured Chatnets
getChatnets :: (MonadError APIError m, MonadIO m) => Stargate -> m (Vector Chatnet)
getChatnets stargate = do
  response <- sendMessage Chatnets stargate
  case eitherDecodeStrict response of
    Left err    -> throwError $ JSONParsingError (toText err)
    Right value -> pure $ chatnetsList value

-- | Query the irssi instance to fetch the available methods
getMethods :: (MonadError APIError m, MonadIO m) => Stargate -> m (Vector Text)
getMethods stargate = do
  response <- sendMessage Methods stargate
  case eitherDecodeStrict response of
    Left err    -> throwError $ JSONParsingError (toText err)
    Right value -> pure value

-- | Query the irssi instance to fetch the irssi version
getVersion :: (MonadError APIError m, MonadIO m) => Stargate -> m Text
getVersion stargate = do
  response <- sendMessage Version stargate
  case eitherDecodeStrict response of
    Left err    -> throwError $ JSONParsingError (toText err)
    Right value -> pure $ value V.! 1
