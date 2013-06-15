{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Spam detection.

module Hpaste.Model.Spam where

import Hpaste.Types
import Data.Monoid
import qualified Data.Text as T

isSpam :: PasteSubmit -> Bool
isSpam PasteSubmit{..} =
  T.isInfixOf "stooorage" allText

  where allText = T.toLower $ pasteSubmitTitle <> " " <> pasteSubmitPaste
