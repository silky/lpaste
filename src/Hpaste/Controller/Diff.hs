{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diff page controller.

module Hpaste.Controller.Diff
  (handle)
  where

import Hpaste.Types
import Hpaste.Controller.Paste (withPasteKey)
import Hpaste.View.Diff        (page)

import Snap.App

-- | Diff one paste with another.
handle :: HPCtrl ()
handle = do
  withPasteKey "this" $ \this ->
    withPasteKey "that" $ \that ->
      output $ page this that
