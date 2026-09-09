{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Server where

import App.AppM
import App.Config
import App.Routes
import Control.Exception.Safe (Exception (displayException), SomeException, try, tryAny)
import Control.Lens ((&), (.~))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader
import Data.Aeson (encode, object, (.=))
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map qualified as Map
import Data.OpenApi
  ( HasInfo (info)
  , HasTitle (title)
  , OpenApi
  )
import Data.Text (pack)
import Data.Text.IO qualified as TIO (hPutStrLn)
import Network.HTTP.Types
  ( Status (statusCode)
  , hContentType
  , internalServerError500
  )
import Network.Wai (Middleware, responseLBS, responseStatus)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders)
  , cors
  , simpleCorsResourcePolicy
  )
import OpenAPI.Orphans ()
import Servant
  ( Application
  , Context (EmptyContext, (:.))
  , ErrorFormatters (urlParseErrorFormatter)
  , Handler (..)
  , HasServer (hoistServerWithContext)
  , NamedRoutes
  , Proxy (Proxy)
  , Server
  , ServerError (errBody)
  , defaultErrorFormatters
  , err400
  , err500
  , serveWithContext
  , type (:<|>) (..)
  )
import Servant.Auth.Server
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServer)
import Servant.Swagger.UI
  ( SwaggerSchemaUI
  , swaggerSchemaUIServer
  )
import System.IO (stderr)

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { urlParseErrorFormatter = \_typeRep _req err ->
        err400
          { errBody =
              encode $
                object
                  [ "error" .= ("invalid_parameter" :: String)
                  , "message" .= err
                  ]
          }
    }

-- catch handler exceptions
catchInternalServerError :: Handler a -> Handler a
catchInternalServerError (Handler action) = Handler $ ExceptT $ do
  result <- tryAny $ runExceptT action
  case result of
    Right r -> pure r
    Left (e :: SomeException) -> do
      liftIO $ TIO.hPutStrLn stderr $ "Unhandled exception: " <> pack (displayException e)
      pure $
        Left
          err500
            { errBody =
                encode $
                  object
                    [ "error" .= ("internal_error" :: String)
                    , "message" .= ("Internal server error" :: String)
                    ]
            }

-- catch routing exceptions
catchRoutingExceprions :: Middleware
catchRoutingExceprions baseApp req rspnd = do
  result <- try $ baseApp req rspnd
  case result of
    Right receipt -> pure receipt
    Left (e :: SomeException) -> do
      liftIO $ TIO.hPutStrLn stderr $ "Unhandled exception: " <> pack (displayException e)
      rspnd $
        responseLBS
          internalServerError500
          [(hContentType, "application/json")]
          ( encode $
              object
                [ "error" .= ("internal_error" :: String)
                , "message" .= ("Internal server error" :: String)
                ]
          )

type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> NamedRoutes Routes

businesServer :: AppConfig -> Routes (AsServer)
businesServer cfg =
  hoistServerWithContext
    (Proxy @(NamedRoutes Routes))
    (Proxy :: Proxy '[ErrorFormatters, JWTSettings, CookieSettings])
    (catchInternalServerError . nt)
    rawBusinessServer
  where
    nt :: AppM a -> Handler a
    nt action = runReaderT (action.runAppM) cfg

openApiDoc :: OpenApi
openApiDoc =
  toOpenApi (Proxy @(NamedRoutes Routes))
    & info . title .~ "User.API"

appServer :: AppConfig -> Servant.Server FullAPI
appServer cfg = swaggerSchemaUIServer openApiDoc :<|> businesServer cfg

customContext :: AppConfig -> Context '[ErrorFormatters, CookieSettings, JWTSettings]
customContext cfg = customFormatters :. cfg.cfgCookieSettings :. cfg.cfgJwtSettings :. EmptyContext

app :: AppConfig -> Application
app cfg = serveWithContext (Proxy @FullAPI) (customContext cfg) $ appServer cfg

corsMW :: Middleware
corsMW = cors $ const $ Just policy
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }

statusMetricsMidldeware :: IORef (Map.Map Int Int) -> Middleware
statusMetricsMidldeware counterRef baseApp req respond = baseApp req $ \res -> do
  modifyIORef' counterRef (Map.insertWith (+) (statusCode (responseStatus res)) 1)
  m <- readIORef counterRef
  putStrLn $ show m
  respond res
