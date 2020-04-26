module Irssi.Utils
  ( say
  ) where

import qualified Data.ByteString.Char8 as C8

say :: Text -> IO ()
say = C8.hPutStrLn stdout . encodeUtf8
