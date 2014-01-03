{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Channel model.

module Hpaste.Model.Channel
  (getChannels)
  where

import Hpaste.Types

import Snap.App

-- | Get the channels.
getChannels :: Model c s [Channel]
getChannels =
  queryNoParams ["SELECT *"
                ,"FROM channel"
                ,"ORDER by title"]
