module Amelie.Types.View
  (Pagination(..))
  where

import Data.Map (Map)
import Data.ByteString (ByteString)
import Network.URI (URI)

-- | Pagination data.
data Pagination = Pagination {
   pnPage :: Integer
 , pnLimit :: Integer
 , pnURI :: URI
 , pnResults :: Integer
 , pnTotal :: Integer
} deriving Show
