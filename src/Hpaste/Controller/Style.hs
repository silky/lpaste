{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stylesheet controller.

module Hpaste.Controller.Style
  (handle)
  where

import Hpaste.Controller (outputText)
import Hpaste.Model
import Hpaste.View.Style (style)

import Snap.Core         (modifyResponse,setContentType)

handle :: Controller ()
handle = do
  modifyResponse $ setContentType "text/css"
  outputText $ style
