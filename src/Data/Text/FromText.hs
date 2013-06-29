module Data.Text.FromText where

import Data.Text
import qualified Data.Text.Lazy as L
import Data.ByteString
import qualified Data.ByteString.Lazy as L

class FromText a where
  fromText :: Text -> Maybe a
  fromLazyText :: L.Text -> Maybe a
  fromLazyText = fromText . L.toStrict
