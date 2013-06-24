-- | Site-wide configuration.

module Hpaste.Types.Config
       (Config(..)
       ,AnnounceConfig(..))
       where

import Database.PostgreSQL.Simple (ConnectInfo)
import Network.Mail.Mime (Address)
import Snap.App.Types

import Hpaste.Types.Announcer

-- | Site-wide configuration.
data Config = Config {
    configAnnounce        :: AnnounceConfig
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

