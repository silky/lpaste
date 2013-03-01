{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diff page controller.

module Hpaste.Controller.Diff
  (handle)
  where

import Hpaste.Controller
import Hpaste.Controller.Paste (withPasteKey)
import Hpaste.Model
import Hpaste.View.Diff        (page)

-- | Diff one paste with another.
handle :: Controller ()
handle = do
  withPasteKey "this" $ \this ->
    withPasteKey "that" $ \that ->
      output $ page this that
