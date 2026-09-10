{-# LANGUAGE OverloadedStrings #-}

module App.AppEff where

import App.Config (AppConfig (..))
import App.Logger
import App.Users
import Effectful
import Effectful.Reader.Static (Reader)

type AppEffects =
  '[ Users
   , Logger
   , Reader AppConfig
   , IOE
   -- , Error AppError
   ]

type AppEff = Eff AppEffects
