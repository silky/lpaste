{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Home page view.

module Hpaste.View.Home
  (page)
  where

import Hpaste.Types
import Hpaste.View.Html
import Hpaste.View.Layout
import Hpaste.View.Paste  (pasteLink)


import Data.Text          (Text)
import Data.Time.Show     (showDateTime)
import Prelude            hiding ((++))
import Text.Blaze.Html5   as H hiding (map)
import qualified Data.Text as T
import Text.Blaze.Extra
import Network.URI.Params
import Network.URI

-- | Render the home page.
page :: URI -> [Channel] -> [Language] -> [Paste] -> Html -> Html
page uri chans langs ps form =
  layoutPage $ Page {
    pageTitle = "Recent pastes"
  , pageBody = content uri chans langs ps form
  , pageName = "home"
  }

-- | Render the home page body.
content :: URI -> [Channel] -> [Language] -> [Paste] -> Html -> Html
content uri chans langs ps form = do
  createNew form
  latest uri chans langs ps

-- | Create a new paste section.
createNew :: Html -> Html
createNew = lightSection "Create new paste"

-- | View the latest pastes.
latest :: URI -> [Channel] -> [Language] -> [Paste] -> Html
latest uri channels languages ps = do
  darkSection "Latest pastes" $ do
    table ! aClass "latest-pastes" $ do
      tr $ mapM_ (th . toHtml) $ words "Title Author When Language Channel"
      pastes ps
    p ! aClass "browse-link" $ browse

    where pastes = mapM_ $ \paste@Paste{..} -> tr $ do
                     td $ pasteLink paste pasteTitle
                     td $ do
                       let author = T.unpack pasteAuthor
		       if True -- validNick author
		       	  then a ! hrefURI (authorUri author) $ toHtml pasteAuthor
			  else toHtml pasteAuthor
                     td $ toHtml $ showDateTime $ pasteDate
                     td $ showLanguage languages pasteLanguage
                     td $ showChannel Nothing channels pasteChannel
          authorUri author = updateUrlParam "author" author
	  	    	   $ updateUrlParam "page"   "0"
			   $ uri { uriPath = "/browse" }

-- | Browse link.
browse :: Html
browse = href ("/browse" :: Text) ("Browse all pastes" :: Text)
