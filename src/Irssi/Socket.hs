module Irssi.Socket where

import           Data.Aeson                (encode)
import           Network.Socket
import           Network.Socket.ByteString

import           Irssi.Types

sendPayload :: Command -> FilePath -> IO ()
sendPayload command path = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix path)
  let payload = (toStrict $ encode command) <> "\n"
  send s payload
  received <- recv s 1024
  print received
  close s

