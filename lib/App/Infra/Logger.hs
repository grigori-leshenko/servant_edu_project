{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Infra.Logger where

import Data.Text (Text)
import Data.Text qualified (show)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

-- import Effectful.Reader.Dynamic (Reader)
import App.Config (AppConfig (cfgLogPrefix))
import Effectful.Reader.Static (Reader, asks)

data Logger :: Effect where
  LogMsg :: Text -> Logger m ()

type instance DispatchOf Logger = Dynamic

makeEffect ''Logger

runLogger :: (IOE :> es, Reader AppConfig :> es) => Eff (Logger : es) a -> Eff es a
runLogger = interpret $ \_ -> \case
  LogMsg msg -> do
    prefix <- asks cfgLogPrefix
    now <- liftIO getCurrentTime
    liftIO $ TIO.putStrLn $ prefix <> " [" <> (Data.Text.show now) <> "] " <> msg
