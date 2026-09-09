{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Routes where

import App.AppM
import App.Config
import App.Errors
import App.UVerbT
import App.Users (User (User, uid), UserId (UserId), getList, runUsers)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (readIORef, writeIORef)
import Data.OpenApi
  ( ToParamSchema
  , ToSchema
  )
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( StdMethod (GET, POST)
  )
import OpenAPI.Orphans ()

import Effectful (runEff)
import Servant (BasicAuthCheck (BasicAuthCheck), BasicAuthResult (..))
import Servant.API (Capture, FromHttpApiData (parseUrlPiece), JSON, ReqBody, (:-), (:>))
import Servant.API.UVerb (UVerb, WithStatus (..))
import Servant.Auth.Server
import Servant.Server.Generic (AsServerT)
import Text.Read

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
        --
        users <- liftIO $ runEff $ runUsers ref getList
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
