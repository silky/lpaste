{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Language model.

module Hpaste.Model.Language
  (getLanguages)
  where

import Hpaste.Types
import Hpaste.Model

-- | Get the languages.
getLanguages :: Model [Language]
getLanguages =
  queryNoParams ["SELECT id,name,title"
                ,"FROM language"
                ,"WHERE visible"
                ,"ORDER BY ordinal,title ASC"]
