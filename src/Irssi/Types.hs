module Irssi.Types where

import           Data.Aeson
import qualified Data.Text   as T
import qualified Data.Vector as V

data Command = Command { cmd     :: Text
                       , network :: Text
                       , channel :: Text
                       , message :: Text
                       } deriving (Show, Eq)


instance ToJSON Command where
  toJSON Command{..} = Array (V.fromList $ [String "command", String commandBody])
    where
      commandBody :: Text
      commandBody = T.intercalate " " [cmd, networkText, channel, message]
      networkText = "-" <> network
