{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Amelie.View.Paste
  (pasteFormlet
  ,page
  ,pasteLink
  ,pasteRawLink)
  where

import           Amelie.Model.Irclogs        (showIrcDateTime)
import           Amelie.Types
import           Amelie.View.Highlight       (highlightPaste)
import           Amelie.View.Hlint           (viewHints)
import           Amelie.View.Html
import           Amelie.View.Layout

import           Control.Applicative       
import           Control.Arrow               ((&&&))
import           Control.Monad               
import           Data.ByteString.UTF8        (toString)
import           Data.List                   (find,nub)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text,pack)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (fromStrict)
import           Data.Time.Show              (showDateTime)
import           Data.Traversable hiding (forM)
import           Numeric
import           Prelude                     hiding ((++))
import           Safe                        (readMay)
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Extra
import           Text.Blaze.Extra
import           Text.Formlet

-- | Render the page page.
page :: PastePage -> Html
page PastePage{ppPaste=p@Paste{..},..} =
  layoutPage $ Page {
    pageTitle = pasteTitle
  , pageBody = do viewPaste (if ppRevision then [] else ppRevisions) [] ppChans ppLangs (p,ppHints)
                  viewAnnotations (p : ppAnnotations)
                                  ppChans
                                  ppLangs
                                  (zip ppAnnotations ppAnnotationHints)
  , pageName = "paste"
  }
  
-- | A formlet for paste submission / annotateing.
pasteFormlet :: PasteFormlet -> (Formlet PasteSubmit,Html)
pasteFormlet pf@PasteFormlet{..} =
  let form = postForm ! A.action (toValue action) $ do
        when pfSubmitted $
          when (not (null pfErrors)) $
            H.div ! aClass "errors" $
              mapM_ (p . toHtml) pfErrors
        formletHtml (pasteSubmit pf) pfParams
        submitInput "submit" "Submit"
  in (pasteSubmit pf,form)
    
  where action = case pfAnnotatePaste of
                   Just Paste{..} -> "/annotate/" ++ show (fromMaybe pasteId pasteParent)
                       where pasteParent = case pasteType of
                               AnnotationOf pid -> Just pid
                               _ -> Nothing
                   Nothing        ->
                     case pfEditPaste of
		       Just Paste{..} -> "/edit/" ++ show pasteId
		       Nothing -> "/new"

-- | The paste submitting formlet itself.
pasteSubmit :: PasteFormlet -> Formlet PasteSubmit
pasteSubmit pf@PasteFormlet{..} =
  PasteSubmit
    <$> pure (getPasteId pf)
    <*> pure (case pfAnnotatePaste of
    	       Just pid -> AnnotationOf (pasteId pid)
	       _ -> case pfEditPaste of
	         Just pid -> RevisionOf (pasteId pid)
		 _ -> NormalPaste)
    <*> req (textInput "title" "Title" (annotateTitle <|> editTitle))
    <*> defaulting "Anonymous Coward" (textInput "author" "Author" Nothing)
    <*> parse (traverse lookupLang)
              (opt (dropInput languages "language" "Language" (snd defChan)))
    <*> parse (traverse lookupChan)
              (opt (dropInput channels "channel" "Channel" (fst defChan)))
    <*> req (areaInput "paste" "Paste" pfContent)
    <*> opt (wrap (H.div ! aClass "spam") (textInput "email" "Email" Nothing))

    where defaulting def = fmap swap where
    	    swap "" = def
	    swap x  = x
    	  channels = options channelName channelName pfChannels
          languages = options languageName languageTitle pfLanguages
          
          lookupLang slug = findOption ((==slug).languageName) pfLanguages languageId
          lookupChan slug = findOption ((==slug).channelName) pfChannels channelId
          
          defChan = maybe (fromMaybe "" (annotateChan <|> editChan)
	  	    	  ,fromMaybe "haskell" (annotateLanguage <|> editLanguage))
                          (channelName &&& trim.channelName)
                          (pfDefChan >>= findChan)
          findChan name = find ((==name).trim.channelName) pfChannels
          trim = T.dropWhile (=='#')
          
          annotateContent = pastePaste <$> pfAnnotatePaste
          annotateTitle = ((++ " (annotation)") . pasteTitle) <$> pfAnnotatePaste
          annotateLanguage = join (fmap pasteLanguage pfAnnotatePaste) >>= findLangById
          annotateChan = join (fmap pasteChannel pfAnnotatePaste) >>= findChanById
 
          editContent = pastePaste <$> pfEditPaste
          editTitle = Nothing
          editLanguage = join (fmap pasteLanguage pfEditPaste) >>= findLangById
          editChan = join (fmap pasteChannel pfEditPaste) >>= findChanById

          findChanById id = channelName <$> find ((==id).channelId) pfChannels
          findLangById id = languageName <$> find ((==id).languageId) pfLanguages

-- | Get the paste id.
getPasteId :: PasteFormlet -> Maybe PasteId
getPasteId PasteFormlet{..} =
  M.lookup "id" pfParams >>=
  readMay . concat . map toString >>=
  return . (fromIntegral :: Integer -> PasteId)

-- | View the paste's annotations.
viewAnnotations :: [Paste] -> [Channel] -> [Language] -> [(Paste,[Hint])] -> Html
viewAnnotations pastes chans langs annotations = do
  mapM_ (viewPaste [] pastes chans langs) annotations

-- | View a paste's details and content.
viewPaste :: [Paste] -> [Paste] -> [Channel] -> [Language] -> (Paste,[Hint]) -> Html
viewPaste revisions annotations chans langs (paste@Paste{..},hints) = do
  pasteDetails revisions annotations chans langs paste
  pasteContent revisions langs paste
  viewHints hints

-- | List the details of the page in a dark section.
pasteDetails :: [Paste] -> [Paste] -> [Channel] -> [Language] -> Paste -> Html
pasteDetails revisions annotations chans langs paste =
  darkNoTitleSection $ do
    pasteNav langs annotations paste
    h2 $ toHtml $ fromStrict (pasteTitle paste)
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ do
        pasteLink paste $ "#" ++ show (pasteId paste)
	" "
        linkToParent paste
      detail "Author(s)" $ do
        let authors | null revisions = map pasteAuthor [paste]
	    	    | otherwise      = map pasteAuthor revisions
        htmlCommasAnd $ flip map (nub authors) $ \author ->
	  linkAuthor author
      detail "Language" $ showLanguage langs (pasteLanguage paste)
      detail "Channel" $ do showChannel chans (pasteChannel paste)
      detail "Created" $ showDateTime (pasteDate paste)
      detail "Raw" $ pasteRawLink paste $ ("View raw link" :: Text)
      unless (length revisions < 2) $ detail "Revisions" $ do
        br
        ul !. "revisions" $ listRevisions paste revisions
    clear

    where detail title content = do
            li $ do strong (title ++ ":"); toHtml content

-- | Link to an author.
linkAuthor :: Text -> Html
linkAuthor author = href ("/browse?author=" ++ author) $ toHtml author

-- | Link to annotation/revision parents.
linkToParent :: Paste -> Html
linkToParent paste = do
  case pasteType paste of
    NormalPaste -> return ()
    AnnotationOf pid -> do "(an annotation of "; pidLink pid; ")"
    RevisionOf pid -> do "(a revision of "; pidLink pid; ")"

-- | List the revisions of a paste.
listRevisions :: Paste -> [Paste] -> Html
listRevisions p [] = return ()
listRevisions p [x] = revisionDetails p x
listRevisions p (x:y:xs) = do
  revisionDetails y x
  listRevisions p (y:xs)

-- | List the details of a revision.
revisionDetails :: Paste -> Paste -> Html
revisionDetails paste revision = li $ do
  toHtml $ showDateTime (pasteDate revision)
  " "
  revisionLink revision $ do "#"; toHtml (show (pasteId revision))
  unless (pasteId paste == pasteId revision) $ do
    " "
    href ("/diff/" ++ show (pasteId paste) ++ "/" ++ show (pasteId revision)) $
      ("(diff)" :: Html)
  ": "
  toHtml (pasteTitle revision)
  " ("
  linkAuthor (pasteAuthor revision)
  ")"

-- | Individual paste navigation.
pasteNav :: [Language] -> [Paste] -> Paste -> Html
pasteNav langs pastes paste =
  H.div ! aClass "paste-nav" $ do
    diffLink
    href ("/edit/" ++ pack (show pid) ++ "") ("Edit" :: Text)
    " - "
    href ("/annotate/" ++ pack (show pid) ++ "") ("Annotate" :: Text)
    " - "
    href ("/report/" ++ pack (show pid) ++ "") ("Report/Delete" :: Text)
    
    where pid = pasteId paste
          pairs = zip (drop 1 pastes) pastes
          parent = fmap snd $ find ((==pid).pasteId.fst) $ pairs
          diffLink =
            case parent of
              Nothing -> return ()
              Just Paste{pasteId=prevId} -> do
                href ("/diff/" ++ show prevId ++ "/" ++ show pid)
                     ("Diff" :: Text)
                " - "
          lang = pasteLanguage paste >>= (`lookup` ls)
          ls = map (languageId &&& languageName) langs

-- | Show the paste content with highlighting.
pasteContent :: [Paste] -> [Language] -> Paste -> Html
pasteContent revisions langs paste =
  case revisions of
    (rev:_) -> lightNoTitleSection $ highlightPaste langs rev
    _ -> lightNoTitleSection $ highlightPaste langs paste

-- | The href link to a paste.
pasteLink :: ToHtml html => Paste -> html -> Html
pasteLink Paste{..} inner = href ("/" ++ show pasteId) inner

-- | The href link to a paste pid.
pidLink :: PasteId -> Html
pidLink pid = href ("/" ++ show pid) $ toHtml $ "#" ++ show pid

-- | The href link to a paste.
revisionLink :: ToHtml html => Paste -> html -> Html
revisionLink Paste{..} inner = href ("/revision/" ++ show pasteId) inner

-- | The href link to a paste, raw content.
pasteRawLink :: ToHtml html => Paste -> html -> Html
pasteRawLink Paste{..} inner = href ("/raw/" ++ show pasteId) inner
