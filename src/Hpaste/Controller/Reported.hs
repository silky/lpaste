{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reported page controller.

module Hpaste.Controller.Reported
  (handle)
  where

import Hpaste.Model.Report   (getSomeReports,countReports)
import Hpaste.Controller.Admin   (withAuth)
import Hpaste.Types
import Hpaste.View.Reported  (page)

import Text.Blaze.Pagination
import Data.Pagination
import Snap.App

-- | List the reported pastes.
handle :: HPCtrl ()
handle =
  withAuth $ \key -> do
    pn <- getPagination "reported"
    total <- model countReports
    reports <- model $ getSomeReports (pnPn pn)
    output $ page pn reports key
