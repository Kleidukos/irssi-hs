{-|
Module      : Irssi.Types
Description : Common types used throughout the library
Copyright   : © Hécate
License     : MIT
Maintainer  : hecate@glitchbra.in
-}

module Irssi.Types
  ( -- * Types
    APIError (..)
  , ChatType (..) -- exported for tests
  , Chatnet (..)
  , ChatnetResponse (..)
  , Command (..)
  , IrssiState(..)
  , Message (..)
  , Server (..)
  , Stargate (..)
  ) where

import           Control.Concurrent.Chan.Unagi
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                     as T
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import qualified Text.Show

-- | This datatype encapsulates the potential JSON parsing error we may
-- encounter when getting data from the socket server
data APIError = JSONParsingError Text
  deriving (Show, Eq)

-- | The newtype holding the communication channels to the worker
newtype Stargate = Stargate {getChans :: (InChan Command, OutChan ByteString)}
  deriving newtype (Eq)

instance Show Stargate where
  show _ = "#<Stargate>"

-- | The worker will attempt to build this state at startup.
data IrssiState =
  IrssiState { methods  :: Vector Text
             , chatnets :: Vector Chatnet
             , version  :: Text
             , path     :: FilePath
             , stargate :: Stargate
             } deriving (Show, Eq)

-- | Intermediate type to aid with JSON decoding
newtype ChatnetResponse = CR { chatnetsList :: Vector Chatnet }
  deriving (Eq, Show)

-- | Metadatas associated with a chatnet
data Chatnet =
  Chatnet { -- | Can be IRC or SILC, will be IRC most of the time
            chatType    :: ChatType
            -- | Name of the Chatnet
          , name        :: Text
            -- | If configured, the nickname appearing in the configuration
          , nick        :: Maybe Text
            -- | If configured, the username appearing in the configuration
          , username    :: Maybe Text
            -- | If configured, the realname appearing in the configuration
          , realname    :: Maybe Text
            -- | If configured, one of the host's IP address you wish to connect to this server from. Irssi will bind to this interface to connect to the
            -- chatnet
          , ownHost     :: Maybe Text
            -- | If configured, a command to be sent when connecting to the chatnet
          , autosendcmd :: Maybe Text
            -- | List of the servers associated with this chatnet
          , servers     :: Maybe (Vector Server)
          } deriving (Show, Generic, Eq)

-- | Metadatas associated wiht a server
data Server =
  Server { -- | The network address of this server
           address        :: Text
           -- | Whether or not your client has been marked as away
         , away           :: Bool
           -- | If the client is marked away, the away reason supplied
         , awayReason     :: Maybe Text
           -- | Whether or not the client is banned from this server
         , banned         :: Bool
           -- | The currently active IRCv3 capabilities
         , capActive      :: Vector Text
           -- | The IRCv3 capabilities supported by the server
         , capSupported   :: HashMap Text Text
           -- | Can be IRC or SILC, will be IRC most of the time
         , chatType       :: ChatType
           -- | Name of the chatnet this server belongs to
         , chatnet        :: Text
           -- | Whether or not the connection has been lost
         , connectionLost :: Bool
           -- | The current nickname your client has on the server
         , nick           :: Text
           -- | The network port used to connect to the server
         , port           :: Int
           -- | The “real” network address the client is connected to.
           -- eg. We may have specified to connect to chat.freenode.net,
           -- but the this hub has redirected us to cherryh.freenode.net
         , realAddress    :: Text
           -- The realname parameter in use for the server. Defaults to the globally-defiend one if no chatnet-specific one has been defined.
         , realname       :: Text
           -- | Whether or not the SASL connection has been successfully established
         , saslSuccess    :: Bool
           -- | Server tag specifies unique tag to refer to the server, usually it’s the same as the IRC network
         , tag            :: Text
           -- | Whether or not this connection uses SSL
         , useSSL         :: Bool
           -- | Whether or not this connection uses TLS
         , useTLS         :: Bool
           -- | The current usermode. For instance: "Zi"
         , usermode       :: Text
           -- | The current nickname your client has on the server
         , username       :: Bool
           -- | The version of the IRCd
         , version        :: Text
         } deriving (Show, Eq)

instance FromJSON ChatnetResponse where
  parseJSON = withArray "payload" $ \arr ->
    CR <$> parseChatnet (V.tail arr)
      where
        parseChatnet arr = traverse parseObject $ extractObject <$> arr
        -- This function extracts the object Chatnet from an Array containing
        -- "Irssi::Irc::Chatnet" and the object of interest.
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
  ownHost    <- o .:? "own_host"
  autosendcmd <- o .:? "autosendcmd"
  let servers = Nothing
  pure Chatnet{..}


-- | The protocol that is associated with the chatnet or server
data ChatType = IRC | SILC
  deriving (FromJSON, Show, Generic, Eq)

-- | A type holding all the necessary information to send a message to someone on IRC
data Message = Message { cmd     :: Text
                       , network :: Text
                       , channel :: Text
                       , message :: Text
                       } deriving (Show, Eq)

-- | The sum type of the exposed API
data Command = Msg Message -- ^ Send a message to someone on IRC
             | Methods     -- ^ Get back a list of supported methods
             | Chatnets    -- ^ Get back a list of configured chatnets
             | Version     -- ^ Get back irssi's version
             | Print Text  -- ^ Print a message as a notification in the status window
             deriving (Show, Eq)

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
  toJSON (Print message)     = Array (V.fromList [String "Irssi::print", String message])
  toJSON (Msg message) = toJSON message
