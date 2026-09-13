{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Config
import App.DBE (DBE, DBError, runBDEIORef)
import App.Errors (AppError)
import App.Logger (Logger, logMsg, runLogger)
import App.Routes (getListHandler)
import App.Server
import App.ServerE (Server, runServer, runServerWarp)
import App.UsersE (Users, UsersError, runUsers)
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Reader.Static (Reader, ask, runReader)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import OpenAPI.Orphans ()
import Servant.Auth.Server

mainEff ::
  Eff
    '[ Server
     , Users
     , DBE
     , Logger
     , Reader AppConfig
     , Error DBError
     , Error UsersError
     , Error AppError
     , IOE
     ]
    ()
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
  _ <-
    runEff
      . runErrorNoCallStack @AppError
      . runErrorNoCallStack @UsersError
      . runErrorNoCallStack @DBError
      . runReader cfg
      . runLogger
      . runBDEIORef ref
      . runUsers
      $ getListHandler
  result <-
    runEff
      . runErrorNoCallStack @AppError
      . runErrorNoCallStack @UsersError
      . runErrorNoCallStack @DBError
      . runReader cfg
      . runLogger
      . runBDEIORef ref
      . runUsers
      . runServerWarp
      $ mainEff
  case result of
    Left err -> putStrLn $ "Fatal application error: " ++ show err
    Right _ -> putStrLn "Server stopped gracefully"
