{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module App.UVerbT (UVerbT (..), runUVerbT, throwUVerb, ErrorBody (..)) where

import App.Errors
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Aeson
import Data.OpenApi
import Data.Text
import GHC.Generics
import Servant (HasStatus, IsMember, Union, WithStatus (WithStatus), respond)
import Servant.API.Status qualified

newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader r)

instance MonadError e m => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h = UVerbT $ ExceptT $ runExceptT act `catchError` (runExceptT . unUVerbT . h)

runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) =
  either id id
    <$> runExceptT
      (act >>= respond)

data ErrorBody = ErrorBody {error :: Text, message :: Text}
  deriving
    ( Show
    , Generic
    , ToJSON
    , ToSchema
    )

mapAppError :: AppError status -> ErrorBody
mapAppError = \case
  InvalidUserName n -> ErrorBody "invalid_name" $ "Name \"" <> pack n <> "\" is not valid"
  UserNotFound uid -> ErrorBody "user_not_found" $ "User with id " <> (pack . Prelude.show $ uid) <> " not found"
  DuplicatedUser n -> ErrorBody "duplicated_user" $ "Name \"" <> pack n <> "\" already exists"
  Denied -> ErrorBody "access_denied" "access denied"
  BadCredentials -> ErrorBody "bad_credentials" "bad credentials"
  TokenCreationFail -> ErrorBody "token_creation_fail" "token creation fail"

throwUVerb ::
  forall s xs m a.
  (Monad m, Servant.API.Status.KnownStatus s, IsMember (WithStatus s ErrorBody) xs) =>
  AppError s -> UVerbT xs m a
throwUVerb e = UVerbT . ExceptT . fmap Left . respond $ WithStatus @s $ mapAppError e
