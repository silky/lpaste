{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Channel model.

module Hpaste.Model.Channel
  (getChannels)
  where

import Hpaste.Types
import Hpaste.Model

-- | Get the channels.
getChannels :: Model [Channel]
getChannels =
  queryNoParams ["SELECT *"
                ,"FROM channel"]
