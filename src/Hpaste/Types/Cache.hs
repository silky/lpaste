{-# OPTIONS -Wall #-}

-- | HTML caching types.

module Hpaste.Types.Cache
       (Key(..)
       ,Cache(..))
       where

import Control.Concurrent.MVar (MVar)
import Data.Map                (Map)
import Data.Text.Lazy          (Text)
import Hpaste.Types.Newtypes

data Key =
    Home
  | Paste PasteId
  | Revision PasteId
  | Activity
    deriving (Eq,Ord)

data Cache =
  Cache {
    cacheMap :: MVar (Map Key Text)
  }
