{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Users (UserId (..), User (..), runUsers, getList) where

import Data.Aeson
import Data.IORef (IORef, readIORef)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import GHC.Generics

newtype UserId = UserId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON
    )

data User = User {uid :: UserId, name :: String}
  deriving
    ( Show
    , Generic
    , ToJSON
    )

data Users :: Effect where
  GetList :: Users m [User]

-- AddUser :: Users m User
-- GetUser :: UserId -> Users m User

type instance DispatchOf Users = Dynamic

makeEffect ''Users

runUsers :: IOE :> es => IORef [User] -> Eff (Users : es) a -> Eff es a
runUsers ref = interpret $ \_ -> \case
  -- GetUser _uid -> pure $ User (UserId 1) "fsd"
  GetList -> do
    users <- liftIO $ readIORef ref
    pure users

-- -- AddUser -> pure $ User (UserId 1) "fsd"
-- testUsers :: Users :> es => Eff es [User]
-- testUsers = do
--   getList

-- runTest :: IO ()
-- runTest = do
--   ref <- newIORef []
--   -- writeIORef ref [User (UserId 1) "gg", User (UserId 2) "kk"]
--   _ <- runEff $ runUsers ref testUsers
--   pure ()