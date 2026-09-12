{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module App.UsersE (UserId (..), User (..), runUsers, getList, Users, getUser, addUser) where

import App.Config (AppConfig (cfgUsersRef))
import App.Errors
import App.Users
import Data.IORef (readIORef, writeIORef)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH (makeEffect)

data Users :: Effect where
  GetList :: Users m [User]
  AddUser :: String -> Users m (Either AppError User)
  GetUser :: UserId -> Users m (Either AppError User)

type instance DispatchOf Users = Dynamic

makeEffect ''Users

runUsers ::
  (IOE :> es, Reader AppConfig :> es, Error AppError :> es) => Eff (Users : es) a -> Eff es a
runUsers = interpret $ \_ -> \case
  GetList -> do
    ref <- asks cfgUsersRef
    users <- liftIO $ readIORef ref
    pure users
  GetUser lookup_uid -> do
    ref <- asks cfgUsersRef
    users <- liftIO $ readIORef ref
    case lookup lookup_uid [(App.Users.uid u, u) | u <- users] of
      Just u -> pure . Right $ u
      Nothing -> throwError $ UserNotFound lookup_uid
  AddUser name -> do
    let vResult = validateName name
    case vResult of
      Left r -> throwError r
      Right _ -> do
        ref <- asks cfgUsersRef
        users <- liftIO $ readIORef ref
        let newId = Prelude.length users + 1
            u = User (UserId newId) name
        liftIO $ writeIORef ref $ u : users
        pure $ Right u

validateName :: [Char] -> Either (AppError) ()
validateName n@(Prelude.null -> True) = Left $ InvalidUserName n
validateName n@((> 50) . Prelude.length -> True) = Left $ InvalidUserName n
validateName _ = Right ()
