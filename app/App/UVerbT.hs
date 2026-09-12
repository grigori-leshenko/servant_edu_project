{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App.UVerbT (UVerbT (..), runUVerbT, ErrorBody (..), throwUVerb, liftEff) where

-- import App.Errors
import Control.Monad.Except
import Control.Monad.Writer.Strict (MonadIO (liftIO), MonadTrans (lift))
import Data.Aeson
import Data.OpenApi
import Data.SOP.BasicFunctors (I (..))
import Data.Text
import Effectful (Eff, IOE, (:>))
import GHC.Generics
import Servant (IsMember, Proxy, Union)

-- import Servant.API.Status qualified

import App.Errors
import Servant.API.Status qualified
import Servant.API.UVerb (WithStatus (..), inject)

newtype UVerbT xs es a = UVerbT {unUVerbT :: ExceptT (Union xs) (Eff es) a}
  deriving newtype (Functor, Applicative, Monad, MonadError (Union xs))

-- instance MonadError e m => MonadError e (UVerbT xs m) where
--   throwError = lift . throwError
--   catchError (UVerbT act) h = UVerbT $ ExceptT $ runExceptT act `catchError` (runExceptT . unUVerbT . h)

instance IOE :> es => MonadIO (UVerbT xs es) where
  liftIO :: IOE :> es => IO a -> UVerbT xs es a
  liftIO = UVerbT . liftIO

runUVerbT ::
  forall xs es a.
  IsMember a xs => UVerbT xs es a -> Eff es (Union xs)
runUVerbT (UVerbT act) = do
  res <- runExceptT act
  pure $ either id (inject . I) res

-- <$> runExceptT act

data ErrorBody = ErrorBody {error :: Text, message :: Text}
  deriving
    ( Show
    , Generic
    , ToJSON
    , ToSchema
    )

mapAppError :: AppError -> ErrorBody
mapAppError = \case
  InvalidUserName n -> ErrorBody "invalid_name" $ "Name \"" <> pack n <> "\" is not valid"
  UserNotFound uid -> ErrorBody "user_not_found" $ "User with id " <> (pack . Prelude.show $ uid) <> " not found"
  DuplicatedUser n -> ErrorBody "duplicated_user" $ "Name \"" <> pack n <> "\" already exists"
  Denied -> ErrorBody "access_denied" "access denied"
  BadCredentials -> ErrorBody "bad_credentials" "bad credentials"
  TokenCreationFail -> ErrorBody "token_creation_fail" "token creation fail"

throwUVerb ::
  forall s xs es a.
  (Servant.API.Status.KnownStatus s, IsMember (WithStatus s ErrorBody) xs) =>
  (Proxy s) -> AppError -> UVerbT xs es a
throwUVerb _ e = UVerbT . ExceptT $ pure $ Left . inject . I $ WithStatus @s $ mapAppError e

liftEff :: Eff es a -> UVerbT xs es a
liftEff = UVerbT . lift
