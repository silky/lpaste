{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reported page controller.

module Hpaste.Controller.Reported
  (handle)
  where

import Hpaste.Model.Report   (getSomeReports,countReports)
import Hpaste.Types
import Hpaste.View.Reported  (page)

import Snap.App

-- | List the reported pastes.
handle :: HPCtrl ()
handle = do
  pn <- getPagination
  total <- model countReports
  reports <- model $ getSomeReports pn
  let pn' = pn { pnResults = fromIntegral (length reports)
               , pnTotal = total }
  output $ page pn' reports
