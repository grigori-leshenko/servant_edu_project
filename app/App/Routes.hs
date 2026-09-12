{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Routes where

-- import App.AppEff
import App.Logger (Logger, logMsg)
import App.UVerbT
import App.Users (User (User, uid), UserId (UserId))
import App.UsersE (Users, getList)

-- import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi
  ( ToParamSchema
  , ToSchema
  )
import Data.Text (Text, pack)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( StdMethod (GET, POST)
  )
import OpenAPI.Orphans ()

import App.AppEff (AppEff)
import App.Config
import App.Errors (AppError (..))
import Control.Monad (unless)
import Control.Monad.Reader (MonadTrans (..))
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Effectful (Eff, MonadIO (liftIO), type (:>))
import Effectful.Reader.Static (asks)
import Servant (BasicAuthCheck (BasicAuthCheck), BasicAuthResult (..))
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
            expirationT <- liftIO $ addUTCTime (60 * 10) <$> getCurrentTime
            jwtS <- liftEff $ asks cfgJwtSettings
            etoken <- liftIO $ makeJWT user jwtS $ Just expirationT
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
                ref <- liftEff $ asks cfgUsersRef
                users <- liftIO $ readIORef ref
                let newId = Prelude.length users + 1
                    u = User (UserId newId) n
                liftIO $ writeIORef ref $ u : users
                pure $ WithStatus @200 $ toWebUser u
          _ -> throwUVerb Denied

    list = runUVerbT $ do
      UVerbT . lift $ logMsg "list"
      users <- liftEff $ getListHandler
      pure $ WithStatus @200 $ toWebUser <$> users

    get_ (WebUserId lookup_uid) ar = do
      logMsg "getUser"
      runUVerbT $ do
        case ar of
          Authenticated _au -> do
            users <- liftEff $ getListHandler
            case lookup (UserId lookup_uid) [(App.Users.uid u, u) | u <- users] of
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
