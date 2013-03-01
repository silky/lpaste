{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Language model.

module Hpaste.Model.Language
  (getLanguages)
  where

import Hpaste.Types

import Snap.App

-- | Get the languages.
getLanguages :: Model c s [Language]
getLanguages =
  queryNoParams ["SELECT id,name,title"
                ,"FROM language"
                ,"WHERE visible"
                ,"ORDER BY ordinal,title ASC"]
