{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report controller.

module Hpaste.Controller.Admin
  (withAuth)
  where

import           Hpaste.Controller.Cache (resetCache)
import           Hpaste.Model.Paste   (getPasteById,deletePaste)
import           Hpaste.Model.Report
import           Hpaste.Types
import           Hpaste.Types.Cache      as Key
import           Hpaste.View.Report
import qualified Hpaste.View.Thanks   as Thanks

import           Control.Applicative
import           Control.Monad.Reader
import           Data.ByteString.UTF8 (toString)
import           Data.Maybe
import           Data.Monoid.Operator ((++))
import           Data.Text            (unpack)
import           Prelude              hiding ((++))
import           Safe
import           Snap.App
import           Text.Blaze.Html5     as H hiding (output,map,body)
import           Text.Formlet

-- | Do something with authority.
withAuth :: (String -> HPCtrl ()) -> HPCtrl ()
withAuth m = do
  key <- fmap (fmap toString) $ getParam "key"
  realkey <- asks (configKey . controllerStateConfig)
  case key of
    Just k | k == realkey -> m k
    _ -> goHome
