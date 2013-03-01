{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The stepeval types.

module Hpaste.Types.Stepeval
       (StepevalPage(..))
       where

import Data.Text              (Text)
import Language.Haskell.HLint

data StepevalPage = StepevalPage {
    sePaste :: Text
  , seHints :: [Suggestion]
}
