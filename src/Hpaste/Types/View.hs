module Hpaste.Types.View
  (Pagination(..))
  where



import Network.URI (URI)

-- | Pagination data.
data Pagination = Pagination {
   pnPage :: Integer
 , pnLimit :: Integer
 , pnURI :: URI
 , pnResults :: Integer
 , pnTotal :: Integer
} deriving Show
