{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Spam detection.

module Hpaste.Model.Spam where

import Hpaste.Types
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import System.Process

-- | Get a spam rating for the given potential paste.
spamRating :: PasteSubmit -> IO Integer
spamRating ps = do
  if definitelySpam ps
     then return 100
     else fmap (weighted ps) (getRating mail)

  where mail = unlines ["from: noreply@hpaste.org"
                       ,"subject: " ++ T.unpack (pasteSubmitTitle ps)
		       ,""
		       ,T.unpack (pasteSubmitPaste ps)]

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
  T.isInfixOf "stooorage" (allText ps)

-- | Multiple the rating by weights specific to hpaste.
weighted :: PasteSubmit -> Integer -> Integer
weighted ps n = foldr ($) n weights where
  weights = [if T.isInfixOf "http://" text || T.isInfixOf "https://" text
  	    	then (* (1 + fromIntegral (T.count "http://" text + T.count "https://" text))) else id
            ,if pasteSubmitAuthor ps == "Anonymous Coward" || pasteSubmitAuthor ps == "Anonymous"
	    	then (*2) else id
            ]
  text = allText ps

-- | Get the text of the paste.
allText :: PasteSubmit -> Text
allText PasteSubmit{..} = T.toLower $ pasteSubmitTitle <> " " <> pasteSubmitPaste

-- | Maximum level, anything equal or above this is treated as definitely spam, ignored.
spamMaxLevel = 100

-- | Minimum level, anything equal or above this is treated as possibly spam, accepted but not listed.
spamMinLevel = 60
