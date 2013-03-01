{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Browse page controller.

module Hpaste.Controller.Browse
  (handle)
  where

import Hpaste.Controller     (output,getPagination,getStringMaybe)
import Hpaste.Model
import Hpaste.Model.Channel  (getChannels)
import Hpaste.Model.Language (getLanguages)
import Hpaste.Model.Paste    (getSomePastes,countPublicPastes)
import Hpaste.View.Browse    (page)

-- | Browse all pastes.
handle :: Controller ()
handle = do
  pn <- getPagination
  author <- getStringMaybe "author"
  total <- model $ countPublicPastes author
  pastes <- model $ getSomePastes author pn
  let pn' = pn { pnResults = fromIntegral (length pastes)
               , pnTotal = total }
  chans <- model getChannels
  langs <- model getLanguages
  output $ page pn' chans langs pastes author
