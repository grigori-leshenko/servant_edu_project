{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Routes where

import App.AppEff (AppEff)
import App.Config
import App.Errors (AppError (..))
import App.ErrorsE (mapError)
import App.Logger (Logger, logMsg)
import App.UVerbT
import App.Users (User (User), UserId (UserId))
import App.UsersE (Users, UsersError (UserNotFound), addUser, getList, getUser)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.OpenApi
  ( ToParamSchema
  , ToSchema
  )
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Effectful (Eff, MonadIO (liftIO), type (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (asks)
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( StdMethod (GET, POST)
  )
import OpenAPI.Orphans ()
import Servant (Proxy (Proxy))
import Servant.API (Capture, FromHttpApiData (parseUrlPiece), JSON, ReqBody, (:-), (:>))
import Servant.API.UVerb (UVerb, WithStatus (..))
import Servant.Auth.Server
import Servant.Server.Generic (AsServerT)
import Text.Read (readMaybe)

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

data Routes mode = Routes
  { login ::
      mode
        :- "login"
          Servant.API.:> ReqBody '[JSON] Creds
          Servant.API.:> UVerb
                           'POST
                           '[JSON]
                           '[WithStatus 200 TokenResponse, WithStatus 401 ErrorBody, WithStatus 403 ErrorBody]
  , addUser ::
      mode
        :- "users"
          Servant.API.:> ReqBody '[JSON] NewUser
          Servant.API.:> Auth
                           '[JWT]
                           AuthedUser
          Servant.API.:> UVerb
                           'POST
                           '[JSON]
                           '[ WithStatus 200 WebUser
                            , WithStatus 400 ErrorBody
                            , WithStatus 409 ErrorBody
                            , WithStatus 403 ErrorBody
                            ]
  , list ::
      mode
        :- "users"
          Servant.API.:> UVerb 'GET '[JSON] '[WithStatus 200 [WebUser], WithStatus 403 ErrorBody, WithStatus 401 ErrorBody]
  , get ::
      mode
        :- "users"
          Servant.API.:> Capture
                           "userId"
                           WebUserId
          Servant.API.:> Auth
                           '[JWT]
                           AuthedUser
          Servant.API.:> UVerb
                           'GET
                           '[JSON]
                           '[WithStatus 200 WebUser, WithStatus 404 ErrorBody, WithStatus 403 ErrorBody]
  , sanityCheck ::
      mode
        :- "sanityCheck" Servant.API.:> UVerb 'GET '[JSON] '[WithStatus 200 (), WithStatus 404 ErrorBody]
  }
  deriving (Generic)

getListHandler :: (Users Effectful.:> es, Logger Effectful.:> es) => Eff es [User]
getListHandler = do
  logMsg "getListHandler"
  users <- getList
  pure users

rawBusinessServer :: Routes (AsServerT AppEff)
rawBusinessServer =
  Routes
    { login = login_
    , addUser = addUser_
    , list = list_
    , get = get_
    , sanityCheck = sanityCheck
    }
  where
    login_ (Creds l p) = do
      logMsg "login"
      runUVerbT $ do
        case findUser l p of
          Nothing -> throwUVerb (Proxy @401) BadCredentials
          Just user -> do
            expirationT <- liftIO $ addUTCTime (60 * 10) <$> getCurrentTime
            jwtS <- liftEff $ asks cfgJwtSettings
            etoken <- liftIO $ makeJWT user jwtS $ Just expirationT
            case etoken of
              Left _err -> throwUVerb (Proxy @401) TokenCreationFail
              Right token ->
                pure $ WithStatus @200 $ TR . decodeUtf8 . BSL.toStrict $ token

    addUser_ (NewUser name) ar = do
      logMsg "addUser"
      runUVerbT $ do
        case ar of
          Authenticated au -> do
            unless au.auIsAdmin $ do
              throwUVerb (Proxy @403) Denied
            res <-
              liftEff $
                runErrorNoCallStack @AppError $
                  mapError UsersError $
                    App.UsersE.addUser name
            case res of
              Left e -> throwUVerb (Proxy @400) e
              Right u -> pure $ WithStatus @200 $ toWebUser u
          _ -> throwUVerb (Proxy @403) Denied

    list_ = runUVerbT $ do
      liftEff $ logMsg "list"
      users <- liftEff $ getListHandler
      pure $ WithStatus @200 $ toWebUser <$> users

    get_ (WebUserId lookup_uid) ar = do
      logMsg "getUser"
      runUVerbT $ do
        case ar of
          Authenticated _au -> do
            res <-
              liftEff $
                runErrorNoCallStack @AppError $
                  mapError UsersError $
                    getUser $
                      UserId lookup_uid
            case res of
              Right u -> pure $ WithStatus @200 $ toWebUser u
              Left e -> throwUVerb (Proxy @404) $ e
          _ -> throwUVerb (Proxy @403) Denied

    sanityCheck = do
      logMsg "sanityCheck"
      runUVerbT $ do
        _ <- throwUVerb (Proxy @404) $ UsersError $ UserNotFound $ UserId 0
        pure $ WithStatus @200 ()
