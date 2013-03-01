{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Activity page controller.

module Hpaste.Controller.Activity
  (handle)
  where

import Hpaste.Controller       (outputText)
import Hpaste.Controller.Cache (cache)
import Hpaste.Model
import Hpaste.Model.Activity   (getCommits)
import Hpaste.Types.Cache      as Key
import Hpaste.View.Activity    (page)

import Control.Monad.Env       (env)

-- | Display commit history.
handle :: Controller ()
handle = do
  html <- cache Key.Activity $ do
    uri <- env $ configCommits . controllerStateConfig
    repourl <- env $ configRepoURL . controllerStateConfig
    commits <- model $ getCommits uri
    return $ Just $ page repourl commits
  maybe (return ()) outputText html
