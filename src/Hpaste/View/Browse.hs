{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Browse page view.

module Hpaste.View.Browse
  (page)
  where

import           Hpaste.Types
import           Hpaste.View.Html
import           Hpaste.View.Layout
import           Hpaste.View.Paste  (pasteLink)

import Data.Pagination
import           Control.Monad
import           Data.Maybe
import           Data.Monoid.Operator
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time.Show     (showDateTime)
import           Network.URI
import           Network.URI.Params
import           Prelude            hiding ((++))
import           Snap.App.Types
import           Text.Blaze.Extra
import           Text.Blaze.Pagination
import           Text.Blaze.Html5   as H hiding (map)

-- | Render the browse page.
page :: PN -> [Channel] -> [Language] -> [Paste] -> Maybe String -> Html
page pn chans langs ps mauthor =
  layoutPage $ Page {
    pageTitle = "Browse pastes"
  , pageBody = browse pn chans langs ps mauthor
  , pageName = "browse"
  }

-- | View the paginated pastes.
browse :: PN -> [Channel] -> [Language] -> [Paste] -> Maybe String -> Html
browse pn channels languages ps mauthor = do
  darkSection title $ do
    pagination pn
    table ! aClass "latest-pastes" $ do
      tr $ mapM_ (th . (toHtml :: String -> Html)) $
	 ["Title"] ++ ["Author"|isNothing mauthor] ++ ["When","Language","Channel"]
      pastes ps
    pagination pn { pnPn = (pnPn pn) { pnShowDesc = False } }

    where pastes = mapM_ $ \paste@Paste{..} -> tr $ do
                     td $ pasteLink paste pasteTitle
                     unless (isJust mauthor) $
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
			   $ pnURI pn
          title = LT.pack $ case mauthor of
	    Just author -> "Pastes by " ++ author
	    Nothing -> "Latest pastes"
