{-# LANGUAGE OverloadedStrings #-}

module App.AppEff where

import App.Config (AppConfig (..))
import App.DBE (DBE, DBError)
import App.Errors
import App.Logger
import App.UsersE
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)

type AppEffects =
  '[ Users
   , DBE
   , Logger
   , Reader AppConfig
   , Error DBError
   , Error UsersError
   , Error AppError
   , IOE
   ]

type AppEff = Eff AppEffects
