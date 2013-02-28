{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | HTML caching.

module Amelie.Controller.Cache
       (newCache
       ,cache
       ,resetCache)
       where
 
import           Amelie.Types (Controller,ControllerState(..))
import           Amelie.Types.Cache
import           Amelie.Types.Config

import           Control.Concurrent
import           Control.Monad.IO         (io)
import           Control.Monad
import           Control.Monad.Reader     (asks)
import qualified Data.Map                 as M
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.IO as T
import           System.Directory
import           Text.Blaze.Html5         (Html)
import           Text.Blaze.Renderer.Text (renderHtml)

-- | Create a new cache.
newCache :: IO Cache
newCache = do
  var <- newMVar M.empty
  return $ Cache var

-- cache :: Key -> Controller (Maybe Html) -> Controller (Maybe Text)
-- cache _key generate = fmap (fmap renderHtml) generate

-- | Generate and save into the cache, or retrieve existing from the
-- | cache.
cache :: Key -> Controller (Maybe Html) -> Controller (Maybe Text)
cache key generate = do
  Cache var <- asks controllerStateCache
  tmpdir <- asks (configCacheDir . controllerStateConfig)
  let cachePath = tmpdir ++ "/" ++ keyToString key
  exists <- io $ doesFileExist cachePath
  if exists
     then do text <- io $ T.readFile cachePath
     	     return (Just text)
     else do text <- fmap (fmap renderHtml) generate
     	     case text of
	       Just text' -> do io $ T.writeFile cachePath text'
	       	    	        return text
               Nothing -> return text

-- | Reset an item in the cache.
resetCache :: Key -> Controller ()
resetCache key = do
  tmpdir <- asks (configCacheDir . controllerStateConfig)
  io $ do
   let cachePath = tmpdir ++ "/" ++ keyToString key
   exists <- io $ doesFileExist cachePath
   when exists $ removeFile cachePath  

keyToString :: Key -> String
keyToString Home = "home"
keyToString Activity = "activity"
keyToString (Paste i) = "paste-" ++ show i
