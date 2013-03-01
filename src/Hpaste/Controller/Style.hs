{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stylesheet controller.

module Hpaste.Controller.Style
  (handle)
  where

import Hpaste.Types
import Hpaste.View.Style (style)

import Snap.App

handle :: HPCtrl ()
handle = do
  modifyResponse $ setContentType "text/css"
  outputText $ style
