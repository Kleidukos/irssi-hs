module Irssi.Socket
  ( sendPayload
  , getChatnets
  , getMethods
  , getVersion
  ) where

import           Data.Aeson                (eitherDecodeStrict, encode)
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Network.Socket
import           Network.Socket.ByteString

import           Irssi.Types

sendPayload :: Command -> FilePath -> IO ByteString
sendPayload command path = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix path)
  let payload = (toStrict $ encode command) <> "\n"
  send s payload
  received <- recv s 8192
  close s
  pure received

getChatnets :: FilePath -> IO (Either Text (Vector Chatnet))
getChatnets path = do
  response <- sendPayload Chatnets path
  let result = eitherDecodeStrict response
  pure $ bimap toText chatnetsList result

getMethods :: FilePath -> IO (Either Text (Vector Text))
getMethods path = do
  response <- sendPayload Methods path
  let result = eitherDecodeStrict response :: Either String (Vector Text)
  pure $ first toText result

getVersion :: FilePath -> IO (Either Text Text)
getVersion path = do
  response <- sendPayload Version path
  let result = eitherDecodeStrict response :: Either String (Vector Text)
  pure $ bimap toText (V.! 1) result
