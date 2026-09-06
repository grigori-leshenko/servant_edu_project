{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Exception.Safe (Exception (displayException), SomeException, try, tryAny)
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.OpenApi
  ( HasInfo (info)
  , HasTitle (title)
  , OpenApi
  , ToParamSchema
  , ToSchema
  )
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO (hPutStrLn, putStrLn)
import Data.Time (addUTCTime, getCurrentTime)
import Errors
import GHC.Generics (Generic)
import Network.HTTP.Types (StdMethod (GET, POST), hContentType, internalServerError500)
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders)
  , cors
  , simpleCorsResourcePolicy
  )
import OpenAPI.Orphans ()
import Servant
  ( Application
  , BasicAuthCheck (BasicAuthCheck)
  , BasicAuthResult (Authorized, Unauthorized)
  , Capture
  , Context (EmptyContext, (:.))
  , ErrorFormatters (urlParseErrorFormatter)
  , FromHttpApiData
  , GenericMode (type (:-))
  , Handler (..)
  , HasServer (hoistServerWithContext)
  , JSON
  , NamedRoutes
  , Proxy (Proxy)
  , ReqBody
  , Server
  , ServerError (errBody)
  , defaultErrorFormatters
  , err400
  , err500
  , serveWithContext
  , type (:<|>) (..)
  , type (:>)
  )
import Servant.API (FromHttpApiData (parseUrlPiece))
import Servant.API.UVerb (UVerb, WithStatus (..))
import Servant.Auth.Server
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServer, AsServerT)
import Servant.Swagger.UI
  ( SwaggerSchemaUI
  , swaggerSchemaUIServer
  )
import System.IO (stderr)
import Text.Read
import UVerbT
import Users (User (User, uid), UserId (UserId))

data AuthedUser = AU {auName :: String, auIsAdmin :: Bool}
  deriving (Show, Generic, ToJSON, FromJSON, FromJWT, ToJWT)

data Creds = Creds
  { credsLogin :: Text
  , credsPass :: Text
  }
  deriving (Show, Generic, FromJSON, ToSchema)

data TokenResponse = TR {accesstoken :: Text}
  deriving (Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

findUser :: Text -> Text -> Maybe AuthedUser
findUser "admin" "1122" = Just $ AU "admin" True
findUser "viewer" "1111" = Just $ AU "viewer" False
findUser _ _ = Nothing

_authCheck :: BasicAuthCheck AuthedUser
_authCheck = BasicAuthCheck auCheck
  where
    auCheck :: BasicAuthData -> IO (BasicAuthResult AuthedUser)
    auCheck (BasicAuthData "admin" "1122") = pure $ Authorized $ AU "admin" True
    auCheck (BasicAuthData "viewer" "ro") = pure $ Authorized $ AU "viever" False
    auCheck _ = pure Unauthorized

newtype WebUserId = WebUserId {unWeb :: Int}
  deriving (Show, Generic)
  deriving newtype (ToJSON, ToSchema, ToParamSchema)

instance FromHttpApiData WebUserId where
  parseUrlPiece :: Text -> Either Text WebUserId
  parseUrlPiece txt = case readMaybe $ Text.unpack txt of
    Just i@((> 0) -> True) -> Right $ WebUserId i
    Just i -> Left $ "UserId should be a positive decimal number, not " <> (pack . Prelude.show $ i)
    Nothing -> Left $ "UserId should be a positive decimal number, not " <> txt

data NewUser = NewUser {name :: String} deriving (Show, Generic, FromJSON, ToSchema)

data WebUser = WebUser {uid :: WebUserId, name :: String}
  deriving (Show, Generic, ToSchema, ToJSON)

toWebUser :: User -> WebUser
toWebUser (User (UserId uid) name) = WebUser (WebUserId uid) name

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

data Routes mode = Routes
  { login ::
      mode
        :- "login"
          :> ReqBody '[JSON] Creds
          :> UVerb
               'POST
               '[JSON]
               '[WithStatus 200 TokenResponse, WithStatus 401 ErrorBody, WithStatus 403 ErrorBody]
  , addUser ::
      mode
        :- "users"
          :> ReqBody '[JSON] NewUser
          :> Auth
               '[JWT]
               AuthedUser
          :> UVerb
               'POST
               '[JSON]
               '[ WithStatus 200 WebUser
                , WithStatus 400 ErrorBody
                , WithStatus 409 ErrorBody
                , WithStatus 403 ErrorBody
                ]
  , list :: mode :- "users" :> UVerb 'GET '[JSON] '[WithStatus 200 [WebUser]]
  , get ::
      mode
        :- "users"
          :> Capture
               "userId"
               WebUserId
          :> Auth
               '[JWT]
               AuthedUser
          :> UVerb
               'GET
               '[JSON]
               '[WithStatus 200 WebUser, WithStatus 404 ErrorBody, WithStatus 403 ErrorBody]
  , sanityCheck ::
      mode :- "sanityCheck" :> UVerb 'GET '[JSON] '[WithStatus 200 (), WithStatus 404 ErrorBody]
  }
  deriving (Generic)

businesServer :: AppConfig -> Routes (AsServer)
businesServer cfg =
  hoistServerWithContext
    (Proxy @(NamedRoutes Routes))
    (Proxy :: Proxy '[ErrorFormatters, JWTSettings, CookieSettings])
    (catchInternalServerError . nt)
    (rawBusinessServer)
  where
    nt :: AppM a -> Handler a
    nt action = runReaderT (action.runAppM) cfg
    rawBusinessServer :: Routes (AsServerT AppM)
    rawBusinessServer =
      Routes
        { login = login_
        , addUser = addUser
        , list = list
        , get = get_
        , sanityCheck = sanityCheck
        }
      where
        login_ (Creds l p) = do
          logMsg "login"
          runUVerbT $ do
            case findUser l p of
              Nothing -> throwUVerb BadCredentials
              Just user -> do
                confg <- asks id
                let jwtS = confg.cfgJwtSettings
                exT <- liftIO $ addUTCTime (60 * 10) <$> getCurrentTime
                etoken <- liftIO $ makeJWT user jwtS $ Just exT
                case etoken of
                  Left _err -> throwUVerb TokenCreationFail
                  Right token ->
                    pure $ WithStatus @200 $ TR . decodeUtf8 . BSL.toStrict $ token

        addUser (NewUser n) ar = do
          logMsg "addUser"
          runUVerbT $ do
            case ar of
              Authenticated au -> do
                unless au.auIsAdmin $ do
                  throwUVerb Denied
                let vResult = validateName n
                case vResult of
                  Left r -> throwUVerb r
                  Right _ -> do
                    config <- asks id
                    let ref = config.cfgUsersRef
                    users <- liftIO $ readIORef ref
                    let newId = Prelude.length users + 1
                        u = User (UserId newId) n
                    liftIO $ writeIORef ref $ u : users
                    pure $ WithStatus @200 $ toWebUser u
              _ -> throwUVerb Denied

        list = do
          logMsg "list"
          runUVerbT $ do
            config <- asks id
            let ref = config.cfgUsersRef
            users <- liftIO $ readIORef ref
            pure $ WithStatus @200 $ toWebUser <$> users

        get_ (WebUserId lookup_uid) ar = do
          logMsg "getUser"
          runUVerbT $ do
            case ar of
              Authenticated _au -> do
                config <- asks id
                let ref = config.cfgUsersRef
                users <- liftIO $ readIORef ref
                case lookup (UserId lookup_uid) [(u.uid, u) | u <- users] of
                  Just u -> pure $ WithStatus @200 $ toWebUser u
                  Nothing -> throwUVerb $ UserNotFound $ UserId lookup_uid
              _ -> throwUVerb $ Denied
        sanityCheck = do
          logMsg "sanityCheck"
          runUVerbT $ do
            _ <- throwUVerb $ UserNotFound $ UserId 0
            pure $ WithStatus @200 ()
        validateName n@(Prelude.null -> True) = Left $ InvalidUserName n
        validateName n@((> 50) . Prelude.length -> True) = Left $ InvalidUserName n
        validateName _ = Right ()

type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> NamedRoutes Routes
data AppConfig = AppConfig
  { cfgUsersRef :: IORef [User]
  , cfgLogPrefix :: Text
  , cfgJwtSettings :: JWTSettings
  , cfgCookieSettings :: CookieSettings
  }

newtype AppM a = AppM {runAppM :: ReaderT AppConfig Handler a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadError ServerError)

logMsg :: Text -> AppM ()
logMsg msg = do
  config <- asks id
  let prefix = config.cfgLogPrefix
  now <- liftIO getCurrentTime
  liftIO $ TIO.putStrLn $ prefix <> " [" <> (T.pack $ show now) <> "] " <> msg

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

main :: IO ()
main = do
  putStrLn "start"
  ref <- newIORef []
  jwk <- generateKey
  putStrLn $ show jwk
  let cfg = AppConfig ref "[dev]" (defaultJWTSettings jwk) defaultCookieSettings
      composedApp =
        corsMW
          . catchRoutingExceprions
          $ app cfg
  run 8888 composedApp
