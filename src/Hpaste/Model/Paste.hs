{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Paste model.

module Hpaste.Model.Paste
  (getLatestPastes
  ,getPasteById
  ,createOrUpdate
  ,createPaste
  ,getAnnotations
  ,getRevisions
  ,getSomePastes
  ,countPublicPastes
  ,generateHints
  ,getHints
  ,validNick)
  where

import Hpaste.Types
import Hpaste.Model.Announcer

import Control.Applicative    ((<$>),(<|>))
import Control.Exception as E
import Control.Monad
import Control.Monad.Env
import Control.Monad.IO
import Data.Char
import Data.List              (find,intercalate)
import Data.Maybe             (fromMaybe,listToMaybe)
import Data.Monoid.Operator   ((++))
import Data.Text              (Text,unpack,pack)
import qualified Data.Text              as T
import Data.Text.IO           as T (writeFile)
import Data.Text.Lazy         (fromStrict)
import Language.Haskell.HLint
import Prelude                hiding ((++))
import Snap.App
import System.Directory
import System.FilePath

-- | Count public pastes.
countPublicPastes :: Maybe String -> HPModel Integer
countPublicPastes mauthor = do
  rows <- single ["SELECT COUNT(*)"
                 ,"FROM public_toplevel_paste"
		 ,"WHERE (? IS NULL) OR (author = ?)"]
		 (mauthor,mauthor)
  return $ fromMaybe 0 rows

-- | Get the latest pastes.
getLatestPastes :: HPModel [Paste]
getLatestPastes =
  queryNoParams ["SELECT *"
                ,"FROM public_toplevel_paste"
                ,"ORDER BY id DESC"
                ,"LIMIT 20"]

-- | Get some paginated pastes.
getSomePastes :: Maybe String -> Pagination -> HPModel [Paste]
getSomePastes mauthor Pagination{..} =
  query ["SELECT *"
	,"FROM public_toplevel_paste"
	,"WHERE (? IS NULL) OR (author = ?)"
	,"ORDER BY id DESC"
	,"OFFSET " ++ show (max 0 (pnPage - 1) * pnLimit)
	,"LIMIT " ++ show pnLimit]
        (mauthor,mauthor)

-- | Get a paste by its id.
getPasteById :: PasteId -> HPModel (Maybe Paste)
getPasteById pid =
  listToMaybe <$> query ["SELECT *"
                        ,"FROM public_paste"
                        ,"WHERE id = ?"]
                        (Only pid)

-- | Get annotations of a paste.
getAnnotations :: PasteId -> HPModel [Paste]
getAnnotations pid =
  query ["SELECT *"
        ,"FROM public_paste"
        ,"WHERE annotation_of = ?"
        ,"ORDER BY id ASC"]
        (Only pid)

-- | Get revisions of a paste.
getRevisions :: PasteId -> HPModel [Paste]
getRevisions pid = do
  query ["SELECT *"
        ,"FROM public_paste"
        ,"WHERE revision_of = ? or id = ?"
        ,"ORDER BY id DESC"]
        (pid,pid)

-- | Create a paste, or update an existing one.
createOrUpdate :: [Language] -> [Channel] -> PasteSubmit -> HPModel (Maybe PasteId)
createOrUpdate langs chans paste@PasteSubmit{..} = do
  case pasteSubmitId of
    Nothing  -> createPaste langs chans paste
    Just pid -> do updatePaste pid paste
                   return $ Just pid

-- | Create a new paste (possibly annotating an existing one).
createPaste :: [Language] -> [Channel] -> PasteSubmit -> HPModel (Maybe PasteId)
createPaste langs chans ps@PasteSubmit{..} = do
  res <- single ["INSERT INTO paste"
                ,"(title,author,content,channel,language,annotation_of,revision_of)"
                ,"VALUES"
                ,"(?,?,?,?,?,?,?)"
                ,"returning id"]
                (pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
                ,pasteSubmitChannel,pasteSubmitLanguage,ann_pid,rev_pid)
  when (lang == Just "haskell") $ just res $ createHints ps
  just (pasteSubmitChannel >>= lookupChan) $ \chan ->
    just res $ \pid -> do
      announcePaste pasteSubmitType (channelName chan) ps pid
  return (pasteSubmitId <|> res)

  where lookupChan cid = find ((==cid).channelId) chans
        lookupLang lid = find ((==lid).languageId) langs
        lang = pasteSubmitLanguage >>= (fmap languageName . lookupLang)
        just j m = maybe (return ()) m j
        ann_pid = case pasteSubmitType of AnnotationOf pid -> Just pid; _ -> Nothing
        rev_pid = case pasteSubmitType of RevisionOf pid -> Just pid; _ -> Nothing

-- | Create the hints for a paste.
createHints :: PasteSubmit -> PasteId -> HPModel ()
createHints ps pid = do
  hints <- generateHintsForPaste ps pid
  forM_ hints $ \hint ->
    exec ["INSERT INTO hint"
         ,"(paste,type,content)"
         ,"VALUES"
         ,"(?,?,?)"]
         (pid
         ,suggestionSeverity hint
         ,show hint)

-- | Announce the paste.
announcePaste :: PasteType -> Text -> PasteSubmit -> PasteId -> HPModel ()
announcePaste ptype channel PasteSubmit{..} pid = do
  conf <- env modelStateConfig
  verb <- getVerb
  unless (seemsLikeSpam pasteSubmitTitle) $
    announce (fromStrict channel) $ fromStrict $ do
      nick ++ " " ++ verb ++ " “" ++ pasteSubmitTitle ++ "” at " ++ link conf
  where nick | validNick (unpack pasteSubmitAuthor) = pasteSubmitAuthor
             | otherwise = "“" ++ pasteSubmitAuthor ++ "”"
        link Config{..} = "http://" ++ pack configDomain ++ "/" ++ pid'
        pid' = case ptype of
	         NormalPaste -> showPid pid
                 AnnotationOf apid -> showPid apid ++ "#a" ++ showPid pid
                 RevisionOf apid -> showPid apid
        getVerb = case ptype of
          NormalPaste -> return $ "pasted"
          AnnotationOf pid -> do
            paste <- getPasteById pid
	    return $ case paste of
	      Just Paste{..} -> "annotated “" ++ pasteTitle ++ "” with"
              Nothing -> "annotated a paste with"
          RevisionOf pid -> do
            paste <- getPasteById pid
	    return $ case paste of
	      Just Paste{..} -> "revised “" ++ pasteTitle ++ "”:"
              Nothing -> "revised a paste:"
        showPid p = pack $ show $ (fromIntegral p :: Integer)
        seemsLikeSpam = T.isInfixOf "http://"

-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: String -> Bool
validNick s = first && all ok s && length s > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
  first = all (\c -> isDigit c || isLetter c) $ take 1 s

-- | Get hints for a Haskell paste from hlint.
generateHintsForPaste :: PasteSubmit -> PasteId -> HPModel [Suggestion]
generateHintsForPaste PasteSubmit{..} (fromIntegral -> pid :: Integer) = io $
  E.catch (generateHints (show pid) pasteSubmitPaste)
          (\SomeException{} -> return [])

-- | Get hints for a Haskell paste from hlint.
generateHints :: FilePath -> Text -> IO [Suggestion]
generateHints pid contents = io $ do
  tmpdir <- getTemporaryDirectory
  let tmp = tmpdir </> pid ++ ".hs"
  exists <- doesFileExist tmp
  unless exists $ T.writeFile tmp $ contents
  !hints <- hlint [tmp,"--quiet","--ignore=Parse error"]
  removeFile tmp
  return hints

getHints :: PasteId -> HPModel [Hint]
getHints pid =
  query ["SELECT type,content"
        ,"FROM hint"
        ,"WHERE paste = ?"]
        (Only pid)

-- | Update an existing paste.
updatePaste :: PasteId -> PasteSubmit -> HPModel ()
updatePaste pid PasteSubmit{..} = do
  _ <- exec (["UPDATE paste"
             ,"SET"]
             ++
             [intercalate ", " (map set (words fields))]
             ++
             ["WHERE id = ?"])
            (pasteSubmitTitle
            ,pasteSubmitAuthor
            ,pasteSubmitPaste
            ,pasteSubmitLanguage
            ,pasteSubmitChannel
            ,pid)
  return ()

    where fields = "title author content language channel"
          set key = unwords [key,"=","?"]
