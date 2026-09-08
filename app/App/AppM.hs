{-# LANGUAGE OverloadedStrings #-}

module App.AppM where

import App.Config (AppConfig (..))
import Control.Monad.Error.Class
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks)
import Data.Text
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Servant

newtype AppM a = AppM {runAppM :: ReaderT AppConfig Handler a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServerError)

logMsg :: Text -> AppM ()
logMsg msg = do
  prefix <- asks cfgLogPrefix
  now <- liftIO getCurrentTime
  liftIO $ TIO.putStrLn $ prefix <> " [" <> (Data.Text.show now) <> "] " <> msg
