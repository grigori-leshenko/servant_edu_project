{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module App.Domain.UsersE (runUsers, getList, Users, getUser, addUser, UsersError (..)) where

import App.Domain.DBE (DBE (..), DBError (NotFound), dBAddUser, dBGetUser, dBGetUsers)
import App.Domain.Users
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)

-- import Effectful.Error.Static (mapError)

import App.Domain.ErrorsE (mapError)
import Effectful.TH (makeEffect)

data UsersError where
  UserNotFound :: UserId -> UsersError
  InvalidUserName :: String -> UsersError
  DuplicatedUser :: String -> UsersError

deriving instance Show UsersError

data Users :: Effect where
  GetList :: Users m [User]
  AddUser :: String -> Users m User
  GetUser :: UserId -> Users m User

type instance DispatchOf Users = Dynamic

makeEffect ''Users

runUsers ::
  (DBE :> es, Error UsersError :> es) => Eff (Users : es) a -> Eff es a
runUsers = interpret $ \_ -> \case
  GetList -> do
    dBGetUsers
  GetUser lookup_uid -> do
    user <- mapError (\(NotFound uid) -> UserNotFound uid) $ dBGetUser lookup_uid
    pure user
  AddUser name -> do
    let vResult = validateName name
    case vResult of
      Left r -> throwError r
      Right _ -> do
        user <- mapError (\_ -> DuplicatedUser name) $ dBAddUser name
        pure user

validateName :: [Char] -> Either (UsersError) ()
validateName n@(Prelude.null -> True) = Left $ InvalidUserName n
validateName n@((> 50) . Prelude.length -> True) = Left $ InvalidUserName n
validateName _ = Right ()
