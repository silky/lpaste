{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Paste controller.

module Hpaste.Controller.Paste
  (handle
  ,pasteForm
  ,getPasteId
  ,getPasteIdKey
  ,withPasteKey)
  where

import Hpaste.Types
import Hpaste.Controller.Cache (cache,resetCache)
import Hpaste.Model.Channel    (getChannels)
import Hpaste.Model.Language   (getLanguages)
import Hpaste.Model.Paste
import Hpaste.Model.Spam
import Hpaste.Types.Cache      as Key
import Hpaste.View.Paste       (pasteFormlet,page)

import Control.Applicative
import Control.Monad           ((>=>))
import Data.ByteString         (ByteString)
import Data.ByteString.UTF8    (toString)
import Data.Maybe
import Data.Monoid.Operator    ((++))
import Data.String             (fromString)
import Data.Text               (Text)
import Prelude                 hiding ((++))
import Safe
import Snap.App
import Text.Blaze.Html5        as H hiding (output)
import Text.Formlet

-- | Handle the paste page.
handle :: Bool -> HPCtrl ()
handle revision = do
  pid <- getPasteId
  justOrGoHome pid $ \(pid :: Integer) -> do
      html <- cache (if revision then Key.Revision pid else Key.Paste pid) $ do
        paste <- model $ getPasteById (fromIntegral pid)
        case paste of
          Nothing -> return Nothing
          Just paste -> do
            hints <- model $ getHints (pasteId paste)
            annotations <- model $ getAnnotations (fromIntegral pid)
            revisions <- model $ getRevisions (fromIntegral pid)
            ahints <- model $ mapM (getHints.pasteId) annotations
            rhints <- model $ mapM (getHints.pasteId) revisions
            chans <- model $ getChannels
            langs <- model $ getLanguages
            return $ Just $ page PastePage {
              ppChans       = chans
            , ppLangs       = langs
            , ppAnnotations = annotations
            , ppRevisions   = revisions
            , ppHints       = hints
            , ppPaste       = paste
            , ppAnnotationHints = ahints
            , ppRevisionsHints = rhints
	    , ppRevision = revision
            }
      justOrGoHome html outputText

-- | Control paste annotating / submission.
pasteForm :: [Channel] -> [Language] -> Maybe Text -> Maybe Paste -> Maybe Paste -> HPCtrl Html
pasteForm channels languages defChan annotatePaste editPaste = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  revisions <- maybe (return []) (model . getRevisions) (fmap pasteId (annotatePaste <|> editPaste))
  let formlet = PasteFormlet {
          pfSubmitted = submitted
        , pfErrors    = []
        , pfParams    = params
        , pfChannels  = channels
        , pfLanguages = languages
        , pfDefChan   = defChan
        , pfAnnotatePaste = annotatePaste
        , pfEditPaste = editPaste
	, pfContent = fmap pastePaste (listToMaybe revisions)
        }
      (getValue,_) = pasteFormlet formlet
      value = formletValue getValue params
      errors = either id (const []) value
      (_,html) = pasteFormlet formlet { pfErrors = errors }
      val = either (const Nothing) Just $ value
  case val of
    Nothing -> return ()
    Just PasteSubmit{pasteSubmitSpamTrap=Just{}} -> goHome
    Just paste | isSpam paste -> goHome
    Just paste -> do
      resetCache Key.Home
      maybe (return ()) (resetCache . Key.Paste . fromIntegral) $ pasteSubmitId paste
      pid <- model $ createPaste languages channels paste
      maybe (return ()) redirectToPaste pid
  return html

-- | Redirect to the paste's page.
redirectToPaste :: PasteId -> HPCtrl ()
redirectToPaste (PasteId pid) =
  redirect $ "/" ++ fromString (show pid)

-- | Get the paste id.
getPasteId :: HPCtrl (Maybe Integer)
getPasteId = (fmap toString >=> readMay) <$> getParam "id"

-- | Get the paste id by a key.
getPasteIdKey :: ByteString -> HPCtrl (Maybe Integer)
getPasteIdKey key = (fmap toString >=> readMay) <$> getParam key

-- | With the
withPasteKey :: ByteString -> (Paste -> HPCtrl a) -> HPCtrl ()
withPasteKey key with = do
  pid <- getPasteIdKey key
  justOrGoHome pid $ \(pid :: Integer) -> do
    paste <- model $ getPasteById (fromIntegral pid)
    justOrGoHome paste $ \paste -> do
      _ <- with paste
      return ()
