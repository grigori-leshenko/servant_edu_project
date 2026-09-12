module App.Config where

import App.Users (User)
import Data.IORef (IORef)
import Data.Map qualified as Map
import Data.Text
import Servant.Auth.Server (JWTSettings)
import Servant.Auth.Server.Internal.ConfigTypes (CookieSettings)

data AppConfig = AppConfig
  { cfgUsersRef :: IORef [User]
  , cfgLogPrefix :: Text
  , cfgJwtSettings :: JWTSettings
  , cfgCookieSettings :: CookieSettings
  , counterRef :: IORef (Map.Map Int Int)
  }