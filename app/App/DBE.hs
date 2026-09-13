{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module App.DBE where

import App.Users
import Data.IORef (IORef, readIORef, writeIORef)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)

data DBError where
  NotFound :: UserId -> DBError

deriving instance Show DBError

data DBE :: Effect where
  DBGetUsers :: DBE m [User]
  DBAddUser :: String -> DBE m User
  DBGetUser :: UserId -> DBE m User

type instance DispatchOf DBE = Dynamic

makeEffect ''DBE

runBDEIORef ::
  (IOE :> es, Error DBError :> es) =>
  IORef [User] -> Eff (DBE : es) a -> Eff es a
runBDEIORef ref = interpret $ \_ -> \case
  DBGetUsers -> do
    users <- liftIO $ readIORef ref
    pure users
  DBGetUser lookup_uid -> do
    users <- liftIO $ readIORef ref
    case lookup lookup_uid [(App.Users.uid u, u) | u <- users] of
      Just u -> pure u
      Nothing -> throwError $ NotFound $ lookup_uid
  DBAddUser name -> do
    users <- liftIO $ readIORef ref
    let newId = Prelude.length users + 1
        u = User (UserId newId) name
    liftIO $ writeIORef ref $ u : users
    pure u
