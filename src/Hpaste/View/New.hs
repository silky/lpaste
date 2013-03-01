{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create new paste view.

module Hpaste.View.New
  (page)
  where

import Hpaste.Types
import Hpaste.View.Html
import Hpaste.View.Layout

import Prelude            hiding ((++))
import Text.Blaze.Html5   as H hiding (map)

-- | Render the create new paste page.
page :: Html -> Html
page form =
  layoutPage $ Page {
    pageTitle = "Create new paste"
  , pageBody = lightSection "Create new paste" form
  , pageName = "new"
  }
