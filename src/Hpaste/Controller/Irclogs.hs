{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Irclogs page controller.

module Hpaste.Controller.Irclogs
  (handle)
  where

import Hpaste.Controller
import Hpaste.Model.Irclogs
import Hpaste.Types
import Hpaste.View.Irclogs  (page)

import Data.String.ToString
import Data.String
import Snap.Types
import Safe

handle :: Controller ()
handle = do
  channel <- get "channel"
  date <- get "date"
  time <- get "timestamp"
  pasteid <- getMaybe "paste"
  logs <- getNarrowedLogs channel date time
  output $ page channel date time logs pasteid

  where get key = do
          value <- fmap (fmap toString) $ getParam (fromString key)
          case value of
            Nothing -> error $ "Missing parameter: " ++ key
            Just value -> return value
        getMaybe key = fmap ((>>= readMay) . fmap toString) $ getParam (fromString key)
