{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IRC announcer.

module Hpaste.Model.Announcer
       (newAnnouncer
       ,announce
       )
       where

import           Hpaste.Types.Announcer
import		 Control.Monad.Fix
import           Control.Concurrent
import qualified Control.Exception       as E
import           Control.Monad
import           Control.Monad.Env       (env)
import           Control.Monad.IO        (io)
import qualified Data.ByteString    as B
import           Data.Monoid.Operator    ((++))
import 		 Data.Char
import           Data.Text          (Text,pack,unpack)
import qualified Data.Text          as T
import           Data.Text.Encoding
import Data.Time
import qualified Data.Text.IO       as T
import           Network
import           Prelude                 hiding ((++))
import           Snap.App.Types
import           System.IO

-- | Start a thread and return a channel to it.
newAnnouncer :: AnnounceConfig -> IO Announcer
newAnnouncer config = do
  putStrLn "Connecting..."
  ans <- newChan
  let self = Announcer { annChan = ans, annConfig = config }
  _ <- forkIO $ announcer self (const (return ()))
  return self

-- | Run the announcer bot.
announcer ::  Announcer -> (Handle -> IO ()) -> IO ()
announcer self@Announcer{annConfig=c@AnnounceConfig{..},annChan=ans} cont = do
  logger "Announcer launched."
  h <- connectTo announceHost (PortNumber $ fromIntegral announcePort)
  hSetBuffering h NoBuffering
  let write h line retry =
        E.catch (do B.hPutStr h (encodeUtf8 (line ++ "\n"))
                    T.putStrLn line)
                (\(e :: IOError) -> do forkIO (announcer self (if retry
                                                                  then (\h -> write h line retry)
                                                                  else const (return ())))
                                       E.throw e)
      send l = write h l False
  logger "Authenticating ..."
  send $ "PASS " ++ pack announcePass
  send $ "USER " ++ pack announceUser ++ " * * *"
  send $ "NICK " ++ pack announceUser
  cont h
  logger "Waiting for demands ..."
  lines <- getChanContents ans
  forM_ lines $ \(Announcement origin line) -> do
    logger ("Whois for " ++ origin ++ ": " ++ line)
    send $ "WHOIS :" ++ origin
    fix $ \loop -> do
      logger ("Waiting for " ++ origin ++ ": " ++ line)
      incoming <- E.catch (T.hGetLine h)
                          (\e -> do logger "* * * * Connection killed. Restarting ..."
                                    forkIO (announcer self (const (return ())))
                                    E.throw (e :: IOError))
      logger ("Got " ++ incoming)
      case T.takeWhile isDigit (T.drop 1 (T.dropWhile (/=' ') incoming)) of
        "311" -> do logger "Writing..."
                    write h line True
	"401" -> logger "Doesn't exist."
	_ -> loop

logger x = do
  t <- getCurrentTime
  appendFile "/tmp/announcer-log.txt" (show t ++ ": " ++ unpack x ++ "\n")

-- | Announce something to the IRC.
announce :: Announcer -> Text -> Text -> Text -> IO ()
announce Announcer{annChan=chan} nick channel line = do
  io $ writeChan chan $ Announcement nick ("PRIVMSG " ++ channel ++ " :" ++ line)
