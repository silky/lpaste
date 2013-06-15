-- | Site-wide configuration.

module Hpaste.Types.Config
       (Config(..)
       ,Announcer(..))
       where

import Database.PostgreSQL.Simple (ConnectInfo)
import Network.Mail.Mime (Address)
import Snap.App.Types

-- | Site-wide configuration.
data Config = Config {
    configAnnounce        :: Announcer
  , configPostgres        :: ConnectInfo
  , configDomain          :: String
  , configCommits         :: String
  , configRepoURL         :: String
  , configIrcDir          :: FilePath
  , configAdmin           :: Address
  , configSiteAddy        :: Address
  , configCacheDir        :: FilePath
  , configKey             :: String
  }

instance AppConfig Config where
  getConfigDomain = configDomain

-- | Announcer configuration.
data Announcer = Announcer {
    announceUser :: String
  , announcePass :: String
  , announceHost :: String
  , announcePort :: Int
} deriving (Show)
