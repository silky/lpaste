module Hpaste.Types.Announcer where

import Control.Concurrent
import Data.Text

-- | Announcer configuration.
data AnnounceConfig = AnnounceConfig {
    announceUser :: String
  , announcePass :: String
  , announceHost :: String
  , announcePort :: Int
} deriving (Show)

-- | An announcer.
data Announcer = Announcer
  { annChan :: Chan Announcement
  , annConfig :: AnnounceConfig
  }

-- | An annoucement.
data Announcement = Announcement
  { annFrom :: Text
  , annContent :: Text
  }
