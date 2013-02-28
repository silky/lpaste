{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Report model.

module Amelie.Model.Report
  (getSomeReports,createReport,countReports)
  where

import Amelie.Types
import Amelie.Model
import Amelie.Controller.Cache
import Amelie.Types.Cache as Key

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Env
import Control.Monad.IO
import Data.Maybe
import Data.Monoid.Operator ((++))
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Prelude              hiding ((++))
import Network.Mail.Mime

-- | Get some paginated reports.
getSomeReports :: Pagination -> Model [Report]
getSomeReports Pagination{..} =
  queryNoParams ["SELECT created,paste,comments"
                ,"FROM report"
                ,"ORDER BY id DESC"
                ,"OFFSET " ++ show (max 0 (pnPage - 1) * pnLimit)
                ,"LIMIT " ++ show pnLimit]

-- | Count reports.
countReports :: Model Integer
countReports = do
  rows <- singleNoParams ["SELECT COUNT(*)"
                         ,"FROM report"]
  return $ fromMaybe 0 rows

-- | Create a new report.
createReport :: ReportSubmit -> Model (Maybe ReportId)
createReport rs@ReportSubmit{..} = do
  res <- single ["INSERT INTO report"
                ,"(paste,comments)"
                ,"VALUES"
                ,"(?,?)"
                ,"returning id"]
                (rsPaste,rsComments)
  _ <- exec ["UPDATE paste"
       	    ,"SET public = false"
	    ,"WHERE id = ?"]
	    (Only rsPaste)
  let reset pid = do
        resetCacheModel (Key.Paste (fromIntegral pid))
        resetCacheModel (Key.Revision (fromIntegral pid))
  reset rsPaste
  sendReport rs
  return res

sendReport ReportSubmit{..} = do
  conf <- env modelStateConfig
  _ <- io $ simpleMail (configAdmin conf)
		       (configSiteAddy conf)
		       (T.pack ("Paste reported: #" ++ show rsPaste))
		       (LT.pack body)
		       (LT.pack body)
		       []
  return ()

  where body = 
  	  "Paste " ++ show rsPaste ++ "\n\n" ++
	  rsComments
