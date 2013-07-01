{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The page type.

module Hpaste.Types.Page
       (Page(..))
       where

import Data.Text  (Text)
import Text.Blaze (Markup)

-- | A page to be rendered in a layout.
data Page = Page {
    pageTitle :: Text
  , pageBody :: Markup
  , pageName :: Text
  }
