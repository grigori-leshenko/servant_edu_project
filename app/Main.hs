{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Config
import App.Logger (Logger, logMsg, runLogger)
import App.Routes (getListHandler)
import App.Server
import App.ServerE (Server, runServer, runServerWarp)
import App.UsersE (Users, runUsers)
import Data.IORef (newIORef)
import Effectful
import Effectful.Reader.Static (Reader, ask, runReader)
import OpenAPI.Orphans ()
import Servant.Auth.Server

mainEff :: Eff '[Server, Users, Logger, Reader AppConfig, IOE] ()
mainEff = do
  logMsg "start main effect"
  cfg <- ask
  let composedApp =
        -- catchRoutingExceprions
        -- . corsMW
        -- . logStdoutDev
        -- . statusMetricsMidldeware counterRef
        app cfg
  runServer 8888 composedApp

main :: IO ()
main = do
  putStrLn "start"
  ref <- newIORef []
  -- counterRef <- newIORef Map.empty
  jwk <- generateKey
  putStrLn $ show jwk
  let cfg = AppConfig ref "[dev]" (defaultJWTSettings jwk) defaultCookieSettings
  -- runEff . runReader cfg . runLogger . runUsers $ getListHandler
  _ <- runEff . runReader cfg . runLogger . runUsers $ getListHandler
  runEff . runReader cfg . runLogger . runUsers . runServerWarp $ mainEff
