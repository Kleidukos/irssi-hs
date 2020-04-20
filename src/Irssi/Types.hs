module Irssi.Types
  ( IrssiState(..)
  , Chatnet (..)
  , ChatnetResponse (..)
  , Command (..)
  , Message (..)
  , parseChatnet -- exported for tests
  , extractObject -- exported for tests
  , ChatType (..) -- exported for tests
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T
import           Data.Vector      (Vector)
import qualified Data.Vector      as V

data IrssiState =
  IrssiState { methods  :: Vector Text
             , chatnets :: Vector Chatnet
             , path     :: FilePath
             , version  :: Text
             } deriving (Show, Eq)

newtype ChatnetResponse = CR { chatnetsList :: Vector Chatnet } deriving (Eq, Show)

data Chatnet = Chatnet { chatType    :: ChatType
                       , name        :: Text
                       , nick        :: Maybe Text
                       , username    :: Maybe Text
                       , realname    :: Maybe Text
                       , own_host    :: Maybe Text
                       , autosendcmd :: Maybe Text
                       , servers     :: Maybe (Vector Text)
                       } deriving (Show, Generic, Eq)

instance FromJSON ChatnetResponse where
  parseJSON = withArray "payload" $ \arr ->
    CR <$> parseChatnet (V.tail arr)

parseChatnet :: Vector Value -> Parser (Vector Chatnet)
parseChatnet arr = traverse parseObject o
  where
    o :: Vector Value
    o = extractObject <$> arr

-- | This function extracts the object Chatnet from an Array containing
--   "Irssi::Irc::Chatnet" and the object of interest.
extractObject :: Value -> Value
extractObject (Array arr) = V.head $ V.tail arr
extractObject _           = error "Ya goofed"

parseObject :: Value -> Parser Chatnet
parseObject = withObject "innerbis" $ \o -> do
  chatType    <- o .:  "chat_type"
  name        <- o .:  "name"
  nick        <- o .:? "nickname"
  username    <- o .:? "username"
  realname    <- o .:? "realname"
  own_host    <- o .:? "own_host"
  autosendcmd <- o .:? "autosendcmd"
  let servers = Nothing
  pure Chatnet{..}

data ChatType = IRC
              | SILC
              deriving (FromJSON, Show, Generic, Eq)

data Message = Message { cmd     :: Text
                       , network :: Text
                       , channel :: Text
                       , message :: Text
                       } deriving (Show, Eq)

data Command = MkMessage Message
             | Methods
             | Chatnets
             | Version
             deriving (Show)

instance ToJSON Message where
  toJSON Message{..} = Array (V.fromList [String "command", String commandBody])
    where
      commandBody :: Text
      commandBody = T.intercalate " " [cmd, networkText, channel, message]
      networkText = "-" <> network

instance ToJSON Command where
  toJSON Methods             = Array (V.fromList [String "methods"])
  toJSON Chatnets            = Array (V.fromList [String "chatnets"])
  toJSON Version             = Array (V.fromList [String "parse_special", String "$J"])
  toJSON (MkMessage message) = toJSON message
