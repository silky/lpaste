module Data.Text.ToText where

import Data.Text
import qualified Data.Text.Lazy as L
import Data.ByteString
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as L

class ToText a where
  toText :: a -> Text
  toLazyText :: a -> L.Text
  toLazyText = L.fromStrict . toText

instance ToText ByteString where
  toText = decodeUtf8
