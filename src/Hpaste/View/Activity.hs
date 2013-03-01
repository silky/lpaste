{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Activity page view.

module Hpaste.View.Activity
  (page)
  where

import Hpaste.Types
import Hpaste.View.Html
import Hpaste.View.Layout

import Control.Monad
import Data.Text          (Text)
import Prelude            hiding ((++))
import Text.Blaze.Html5   as H hiding (map)

-- | Render the activity page.
page :: String -> [Commit] -> Html
page repo commits =
  layoutPage $ Page {
    pageTitle = "Development activity"
  , pageBody = activity repo commits
  , pageName = "activity"
  }

-- | View the paginated pastes.
activity :: String -> [Commit] -> Html
activity repo commits = do
  darkSection "Development activity" $ do
    p $ do "Repository: "
           href repo repo
  forM_ commits $ \Commit{..} -> do
    lightSection commitTitle $ do
      p $ toHtml $ show commitDate
      p $ href commitLink ("Go to diff" :: Text)
