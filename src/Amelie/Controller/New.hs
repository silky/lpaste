{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new paste controller.

module Amelie.Controller.New
  (handle,NewStyle(..))
  where

import Amelie.Controller
import Amelie.Controller.Paste (pasteForm,getPasteId)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.Model.Paste      (getPasteById)
import Amelie.View.Annotate    as Annotate (page)
import Amelie.View.Edit        as Edit (page)
import Amelie.View.New         as New (page)

import Control.Applicative
import Data.Text.Encoding      (decodeUtf8)
import Snap.Core

data NewStyle = NewPaste | AnnotatePaste | EditPaste
 deriving Eq

-- | Make a new paste.
handle :: NewStyle -> Controller ()
handle style = do
  chans <- model $ getChannels
  langs <- model $ getLanguages
  defChan <- fmap decodeUtf8 <$> getParam "channel"
  pid <- if style == NewPaste then return Nothing else getPasteId
  case pid of
    Just pid -> do
      paste <- model $ getPasteById (fromIntegral pid)
      let apaste | style == AnnotatePaste = paste
      	  	 | otherwise = Nothing
      let epaste | style == EditPaste = paste
      	  	 | otherwise = Nothing
      form <- pasteForm chans langs defChan apaste epaste
      justOrGoHome paste $ \paste -> do
        case style of
          AnnotatePaste -> output $ Annotate.page paste form
	  EditPaste     -> output $ Edit.page paste form
	  _ -> goHome
    Nothing -> do
      form <- pasteForm chans langs defChan Nothing Nothing
      output $ New.page form
