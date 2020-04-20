module Irssi
  ( sendPayload
  , Message (..)
  , Command (..)
  , initState
  ) where

import           System.Directory

import           Irssi.Socket
import           Irssi.Types

initState :: IO (Either Text IrssiState)
initState = do
  home <- getHomeDirectory
  let path = home <> "/.irssi.sock"
  chatnets <- getChatnets path
  methods  <- getMethods path
  version  <- getVersion path
  let irssi = IrssiState
          <$> methods
          <*> chatnets
          <*> (Right path)
          <*> version
  pure irssi
