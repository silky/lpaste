{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module Hpaste.Controller.Home
  (handle)
  where

import Hpaste.Controller       (outputText,getMyURI)
import Hpaste.Controller.Cache (cache)
import Hpaste.Controller.Paste (pasteForm)
import Hpaste.Model
import Hpaste.Model.Channel    (getChannels)
import Hpaste.Model.Language   (getLanguages)
import Hpaste.Model.Paste      (getLatestPastes)
import Hpaste.Types.Cache      as Key
import Hpaste.View.Home        (page)

-- | Handle the home page, display a simple list and paste form.
handle :: Controller ()
handle = do
  html <- cache Key.Home $ do
    pastes <- model $ getLatestPastes
    chans <- model $ getChannels
    langs <- model $ getLanguages
    form <- pasteForm chans langs Nothing Nothing Nothing
    uri <- getMyURI
    return $ Just $ page uri chans langs pastes form
  maybe (return ()) outputText html
