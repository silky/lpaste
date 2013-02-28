{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Annotate paste view.

module Amelie.View.Annotate
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout

import Data.Monoid.Operator ((++))
import Prelude              hiding ((++))
import Text.Blaze.Html5     as H hiding (map)
import Data.Text.Lazy

-- | Render the create annotate paste page.
page :: Paste -> Html -> Html
page Paste{..} form =
  layoutPage $ Page {
    pageTitle = "Annotate: " ++ pasteTitle
  , pageBody = lightSection ("Annotate: " ++ fromStrict pasteTitle) form
  , pageName = "annotate"
  }
