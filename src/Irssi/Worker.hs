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
import           Data.Aeson                    (eitherDecodeStrict, encode)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Network.Socket
import           Network.Socket.ByteString

import           Irssi.Types

-- | Start a background worker that will keep the connection to
--   the UNIX-domain socket.
--   This function returns a ''writer' chan, in which Command command
--   can be sent, and a ''reader' chan, from which the responses
--   will be received.
--
--   === __Example:__
--   > startWorker "/home/user/.irssi.sock"
startWorker :: FilePath -> IO (Either Text IrssiState)
startWorker path = do
  (workerWriter, mainReader) <- newChan :: IO (InChan ByteString, OutChan ByteString)
  (mainWriter, workerReader) <- newChan :: IO (InChan Command, OutChan Command)
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix path)
  forkIO $ process (workerWriter, workerReader) s
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
sendMessage :: Command -> Stargate -> IO ByteString
sendMessage command stargate = do
  let (writerChan, readerChan) = getChans stargate
  writeChan writerChan command
  readChan readerChan

-- | Query the
makeState :: Stargate -> FilePath -> IO (Either Text IrssiState)
makeState stargate path = do
  methods <- getMethods stargate
  chatnets <- getChatnets stargate
  version <- getVersion stargate
  let irssiState = IrssiState <$> methods
                              <*> chatnets
                              <*> version
                              <*> pure path
                              <*> pure stargate
  pure irssiState

getChatnets :: Stargate -> IO (Either Text (Vector Chatnet))
getChatnets stargate = do
  response <- sendMessage Chatnets stargate
  let result = eitherDecodeStrict response
  pure $ bimap toText chatnetsList result

getMethods :: Stargate -> IO (Either Text (Vector Text))
getMethods stargate = do
  response <- sendMessage Methods stargate
  let result = eitherDecodeStrict response
  pure $ first toText result

getVersion :: Stargate -> IO (Either Text Text)
getVersion stargate = do
  response <- sendMessage Version stargate
  let result = eitherDecodeStrict response
  pure $ bimap toText (V.! 1) result
