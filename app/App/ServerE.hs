{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.ServerE where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp

data Server :: Effect where
  RunServer :: Warp.Port -> Application -> Server m ()

type instance DispatchOf Server = Dynamic
makeEffect ''Server

runServerWarp :: IOE :> es => Eff (Server : es) a -> Eff es a
runServerWarp = interpret $ \_ -> \case
  RunServer port app -> liftIO $ Warp.run port app