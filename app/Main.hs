{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Config
import App.Server
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import OpenAPI.Orphans ()
import Servant.Auth.Server

main :: IO ()
main = do
  putStrLn "start"
  ref <- newIORef []
  counterRef <- newIORef Map.empty
  jwk <- generateKey
  putStrLn $ show jwk
  let cfg = AppConfig ref "[dev]" (defaultJWTSettings jwk) defaultCookieSettings
      composedApp =
        catchRoutingExceprions
          . corsMW
          . logStdoutDev
          . statusMetricsMidldeware counterRef
          $ app cfg
  run 8888 composedApp
