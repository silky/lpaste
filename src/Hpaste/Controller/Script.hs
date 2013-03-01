{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript controller.

module Hpaste.Controller.Script
  (handle)
  where

import Hpaste.Controller  (outputText)
import Hpaste.Model
import Hpaste.View.Script (script)

import Snap.Core          (modifyResponse,setContentType)

handle :: Controller ()
handle = do
  modifyResponse $ setContentType "text/javascript"
  outputText $ script
