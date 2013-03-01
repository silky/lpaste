{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reported page controller.

module Hpaste.Controller.Reported
  (handle)
  where

import Hpaste.Controller     (output,getPagination)
import Hpaste.Model
import Hpaste.Model.Report   (getSomeReports,countReports)
import Hpaste.View.Reported  (page)

-- | List the reported pastes.
handle :: Controller ()
handle = do
  pn <- getPagination
  total <- model countReports
  reports <- model $ getSomeReports pn
  let pn' = pn { pnResults = fromIntegral (length reports)
               , pnTotal = total }
  output $ page pn' reports
