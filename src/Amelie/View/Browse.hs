{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Browse page view.

module Amelie.View.Browse
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout
import Amelie.View.Paste  (pasteLink)
import Amelie.Model.Paste (validNick)

import Control.Monad
import Data.Maybe
import Data.Time.Show     (showDateTime)
import Prelude            hiding ((++))
import Data.Monoid.Operator
import Text.Blaze.Html5   as H hiding (map)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Blaze.Extra
import Network.URI.Params

-- | Render the browse page.
page :: Pagination -> [Channel] -> [Language] -> [Paste] -> Maybe String -> Html
page pn chans langs ps mauthor =
  layoutPage $ Page {
    pageTitle = "Browse pastes"
  , pageBody = browse pn chans langs ps mauthor
  , pageName = "browse"
  }

-- | View the paginated pastes.
browse :: Pagination -> [Channel] -> [Language] -> [Paste] -> Maybe String -> Html
browse pn channels languages ps mauthor = do
  darkSection title $ do
    paginate pn $ do
      table ! aClass "latest-pastes" $ do
        tr $ mapM_ (th . (toHtml :: String -> Html)) $
	   ["Title"] ++ ["Author"|isNothing mauthor] ++ ["When","Language","Channel"]
        pastes ps

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
                     td $ showChannel channels pasteChannel
          authorUri author = updateUrlParam "author" author
	  	    	   $ updateUrlParam "page"   "0"
			   $ pnURI pn
          title = LT.pack $ case mauthor of
	    Just author -> "Pastes by " ++ author
	    Nothing -> "Latest pastes"