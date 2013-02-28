{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Browse page controller.

module Amelie.Controller.Browse
  (handle)
  where

import Amelie.Controller     (output,getPagination,getStringMaybe)
import Amelie.Model
import Amelie.Model.Channel  (getChannels)
import Amelie.Model.Language (getLanguages)
import Amelie.Model.Paste    (getSomePastes,countPublicPastes)
import Amelie.View.Browse    (page)

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
  output $ page pn' chans langs pastes
