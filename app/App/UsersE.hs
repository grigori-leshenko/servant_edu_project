{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.UsersE (UserId (..), User (..), runUsers, getList, Users) where

import App.Config (AppConfig (cfgUsersRef))
import App.Users
import Data.IORef (readIORef)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH (makeEffect)

data Users :: Effect where
  GetList :: Users m [User]

-- AddUser :: Users m User
-- GetUser :: UserId -> Users m User

type instance DispatchOf Users = Dynamic

makeEffect ''Users

runUsers :: (IOE :> es, Reader AppConfig :> es) => Eff (Users : es) a -> Eff es a
runUsers = interpret $ \_ -> \case
  -- GetUser _uid -> pure $ User (UserId 1) "fsd"
  GetList -> do
    ref <- asks cfgUsersRef
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