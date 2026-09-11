{-# LANGUAGE OverloadedStrings #-}

module App.AppEff where

import App.Config (AppConfig (..))
import App.Errors
import App.Logger
import App.UsersE
import Effectful
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Static (Reader)

type AppEffects =
  '[ Users
   , Error AppError'
   , Logger
   , Reader AppConfig
   , IOE
   ]

type AppEff = Eff AppEffects
