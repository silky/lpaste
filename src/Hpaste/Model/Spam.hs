{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Hpaste.Model.Spam where

import Hpaste.Types
import Data.Monoid
import Data.Text (Text)
import Control.Monad.IO
import Control.Monad.Env
import Control.Monad
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import System.Process hiding (env)
import Snap.App
import Network.Mail.Mime

-- | Get a spam rating for the given potential paste.
spamRating :: PasteSubmit -> Model Config s Integer
spamRating ps = do
  score <- if definitelySpam ps
     then return 100
     else fmap (weighted ps) (io (getRating mail))
  when (score > spamMaxLevel) $ reportBadScore ps score
  return score

  where mail = unlines ["from: noreply@hpaste.org"
                       ,"subject: " ++ T.unpack (pasteSubmitTitle ps)
		       ,""
		       ,T.unpack (pasteSubmitPaste ps)]

reportBadScore PasteSubmit{..} score = do
  conf <- env modelStateConfig
  m <- io $ simpleMail (configAdmin conf)
		       (configSiteAddy conf)
		       ("Paste marked as spam: " <> pasteSubmitTitle)
		       body
		       body
		       []
  io $ renderSendMail m

  where body = LT.pack $
  	  "Paste '" ++ T.unpack pasteSubmitTitle ++ "' by " ++ T.unpack pasteSubmitAuthor ++ " " ++
	  "has rating " ++ show score ++ " with content: " ++
	  T.unpack pasteSubmitPaste

-- | Get the rating from spam assassin.
getRating :: String -> IO Integer
getRating mail = do 
  (_,err,_) <- readProcessWithExitCode "spamc" ["-c"] mail
  return $ case reads err of
    [(n :: Double,_)]     -> round (n*10)
    _                     -> 50

-- | Mark something as definitely spam.
definitelySpam :: PasteSubmit -> Bool
definitelySpam ps =
  T.isInfixOf "stooorage" (allText ps) ||
  T.isInfixOf "http://fur.ly" (allText ps) ||
  justUrl 
   where justUrl = 
   	   (T.isPrefixOf "http://" paste ||
   	    T.isPrefixOf "https://" paste) &&
	    lineCount == 1
	 lineCount = length (filter (not . T.null) 
                                    (map T.strip
                                         (T.lines paste)))
         paste = T.strip (pasteSubmitPaste ps)

-- | Multiple the rating by weights specific to hpaste.
weighted :: PasteSubmit -> Integer -> Integer
weighted ps n = foldr ($) n weights where
  weights = [if T.isInfixOf "http://" text || T.isInfixOf "https://" text
  	    	then (+ (20 * fromIntegral (T.count "http://" text + T.count "https://" text))) else id
            ,if pasteSubmitAuthor ps == "Anonymous Coward" || pasteSubmitAuthor ps == "Anonymous"
	    	then (+20) else id
            ]
  text = allText ps

-- | Get the text of the paste.
allText :: PasteSubmit -> Text
allText PasteSubmit{..} = T.toLower $ pasteSubmitTitle <> " " <> pasteSubmitPaste

-- | Maximum level, anything equal or above this is treated as definitely spam, ignored.
spamMaxLevel = 100

-- | Minimum level, anything equal or above this is treated as possibly spam, accepted but not listed.
spamMinLevel = 60
