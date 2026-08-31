{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Exception.Safe (Exception (displayException), SomeException, try, tryAny)
import Control.Lens ((&), (.~))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, encode, object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.OpenApi
  ( HasInfo (info)
  , HasTitle (title)
  , OpenApi
  , ToParamSchema
  , ToSchema
  )
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.IO (hPutStrLn)
import GHC.Generics (Generic)
import Network.HTTP.Types (StdMethod (GET, POST), hContentType, internalServerError500)
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application
  , Capture
  , Context (EmptyContext, (:.))
  , ErrorFormatters (urlParseErrorFormatter)
  , FromHttpApiData (..)
  , GenericMode (type (:-))
  , Handler (..)
  , IsMember
  , JSON
  , NamedRoutes
  , Proxy (Proxy)
  , ReqBody
  , Server
  , ServerError (errBody)
  , defaultErrorFormatters
  , err400
  , err404
  , err409
  , err500
  , hoistServer
  , serveWithContext
  , throwError
  , type (:<|>) (..)
  , type (:>)
  )
import Servant.API.UVerb (UVerb, WithStatus (..))
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServer)
import Servant.Swagger.UI
  ( SwaggerSchemaUI
  , swaggerSchemaUIServer
  )
import System.IO (stderr)
import Text.Read (readMaybe)
import UVerbT

newtype UserId = UserId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToSchema, ToParamSchema)

instance FromHttpApiData UserId where
  parseUrlPiece :: Text -> Either Text UserId
  parseUrlPiece txt = case readMaybe $ Text.unpack txt of
    Just i@((> 0) -> True) -> Right $ UserId i
    Just i -> Left $ "UserId should be a positive decimal number, not " <> (pack . Prelude.show $ i)
    Nothing -> Left $ "UserId should be a positive decimal number, not " <> txt

data User = User {uid :: UserId, name :: String} deriving (Show, Generic, ToJSON, ToSchema)

data NewUser = NewUser {name :: String} deriving (Show, Generic, FromJSON, ToSchema)

data AppError = UserNotFound UserId | InvalidUserName String | DuplicatedUser String deriving (Show)

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

_toServerError :: AppError -> ServerError
_toServerError (UserNotFound uid) =
  err404
    { errBody =
        encode $
          object
            [ "error" .= ("user_not_found" :: String)
            , "message" .= ("User with id " <> Prelude.show uid <> " not found")
            ]
    }
_toServerError (InvalidUserName n) =
  err400
    { errBody =
        encode $
          object
            [ "error" .= ("invalid_name" :: String)
            , "message" .= ("Name \"" <> n <> "\" is not valid")
            ]
    }
_toServerError (DuplicatedUser n) =
  err409
    { errBody =
        encode $
          object
            [ "error" .= ("duplicated_user" :: String)
            , "message" .= ("Name \"" <> n <> "\" already exists")
            ]
    }

-- catch handler exceptions
catchInternalServerError :: Handler a -> Handler a
catchInternalServerError (Handler action) = Handler $ ExceptT $ do
  result <- tryAny $ runExceptT action
  case result of
    Right r -> pure r
    Left (e :: SomeException) -> do
      liftIO $ hPutStrLn stderr $ "Unhandled exception: " <> pack (displayException e)
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
      liftIO $ hPutStrLn stderr $ "Unhandled exception: " <> pack (displayException e)
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

_throwApp :: AppError -> Handler addUser
_throwApp = throwError . _toServerError

mapAppError ::
  ( IsMember (WithStatus 400 ErrorBody) xs
  , IsMember (WithStatus 404 ErrorBody) xs
  , IsMember (WithStatus 409 ErrorBody) xs
  , Monad m
  ) =>
  AppError -> UVerbT xs m a
mapAppError = \case
  InvalidUserName n ->
    throwUVerb $ WithStatus @400 (ErrorBody "invalid_name" $ "Name \"" <> pack n <> "\" is not valid")
  UserNotFound uid ->
    throwUVerb $
      WithStatus @404
        (ErrorBody "user_not_found" $ "User with id " <> (pack . Prelude.show $ uid) <> " not found")
  DuplicatedUser n ->
    throwUVerb $
      WithStatus @409 (ErrorBody "duplicated_user" $ "Name \"" <> pack n <> "\" already exists")

data ErrorBody = ErrorBody {error :: Text, message :: Text}
  deriving (Show, Generic, ToJSON, ToSchema)

data Routes mode = Routes
  { addUser ::
      mode
        :- "users"
          :> ReqBody '[JSON] NewUser
          :> UVerb
               'POST
               '[JSON]
               '[WithStatus 200 User, WithStatus 400 ErrorBody, WithStatus 404 ErrorBody, WithStatus 409 ErrorBody]
  , list :: mode :- "users" :> UVerb 'GET '[JSON] '[WithStatus 200 [User], WithStatus 400 ErrorBody]
  , get ::
      mode
        :- "users"
          :> Capture "userId" UserId
          :> UVerb
               'GET
               '[JSON]
               '[WithStatus 200 User, WithStatus 404 ErrorBody, WithStatus 400 ErrorBody, WithStatus 409 ErrorBody]
  }
  deriving (Generic)

businesServer :: IORef [User] -> Routes AsServer
businesServer ref_ = hoistServer (Proxy @(NamedRoutes Routes)) catchInternalServerError (rawBusinessServer ref_)
  where
    rawBusinessServer ref =
      Routes
        { addUser = addUser
        , list = list
        , get = get_
        }
      where
        addUser (NewUser n) = runUVerbT $ do
          let vResult = validateName n
          case vResult of
            Left r -> mapAppError r
            Right _ -> do
              users <- liftIO $ readIORef ref
              let newId = Prelude.length users + 1
                  u = User (UserId newId) n
              liftIO $ writeIORef ref $ u : users
              pure $ WithStatus @200 u
        list = runUVerbT $ do
          users <- liftIO $ readIORef ref
          pure $ WithStatus @200 users
        get_ lookup_uid = runUVerbT $ do
          users <- liftIO $ readIORef ref
          case lookup lookup_uid [(u.uid, u) | u <- users] of
            Just u -> pure $ WithStatus @200 u
            Nothing -> mapAppError $ UserNotFound lookup_uid
        validateName n@(Prelude.null -> True) = Left $ InvalidUserName n
        validateName n@((> 50) . Prelude.length -> True) = Left $ InvalidUserName n
        validateName _ = Right ()

type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> NamedRoutes Routes

openApiDoc :: OpenApi
openApiDoc =
  toOpenApi (Proxy @(NamedRoutes Routes))
    & info . title .~ "User.API"

appServer :: IORef [User] -> Servant.Server FullAPI
appServer ref = swaggerSchemaUIServer openApiDoc :<|> businesServer ref

customContext :: Context '[ErrorFormatters]
customContext = customFormatters :. EmptyContext

app :: IORef [User] -> Application
app ref = serveWithContext (Proxy @FullAPI) customContext $ appServer ref

main :: IO ()
main = do
  putStrLn "start"
  ref <- newIORef []
  run 8888 $ catchRoutingExceprions $ app ref
