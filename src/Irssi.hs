{-|
Module      : Irssi
Description : Main interface to communicate with irssi
Copyright   : © Hécate
License     : MIT
Maintainer  : hecate@glitchbra.in
-}

module Irssi
  ( -- * Types
    Message (..)
  , Command (..)
    -- * Functions
  , startWorker
  , sendMessage
  ) where

import           Irssi.Types
import           Irssi.Worker
