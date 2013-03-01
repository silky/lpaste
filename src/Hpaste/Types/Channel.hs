{-# OPTIONS -Wall #-}

-- | The channel type.

module Hpaste.Types.Channel
       (Channel(..))
       where

import Hpaste.Types.Newtypes

import Data.Text                               (Text)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))

data Channel = Channel {
  channelId   :: ChannelId
 ,channelName :: Text
} deriving Show

instance QueryResults Channel where
  convertResults field values = Channel {
      channelId = cid
    , channelName = name
    }
    where (cid,name) = convertResults field values
