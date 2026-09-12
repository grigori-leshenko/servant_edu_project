{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Config
import App.Logger (Logger, logMsg, runLogger)
import App.Routes (getListHandler)
import App.Server
import App.ServerE (Server, runServer, runServerWarp)
import App.UsersE (Users, runUsers)
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Effectful
import Effectful.Reader.Static (Reader, ask, runReader)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import OpenAPI.Orphans ()
import Servant.Auth.Server

mainEff :: Eff '[Server, Users, Logger, Reader AppConfig, IOE] ()
mainEff = do
  logMsg "start main effect"
  cfg <- ask
  let composedApp =
        catchRoutingExceprions
          . corsMW
          . logStdoutDev
          . statusMetricsMidldeware (counterRef cfg)
          $ app cfg
  runServer 8888 composedApp

main :: IO ()
main = do
  putStrLn "start"
  ref <- newIORef []
  jwk <- generateKey
  counterRef <- newIORef Map.empty
  let cfg =
        AppConfig
          { cfgUsersRef = ref
          , cfgLogPrefix = "[dev]"
          , cfgJwtSettings = (defaultJWTSettings jwk)
          , cfgCookieSettings = defaultCookieSettings
          , counterRef = counterRef
          }
  _ <- runEff . runReader cfg . runLogger . runUsers $ getListHandler
  runEff . runReader cfg . runLogger . runUsers . runServerWarp $ mainEff
