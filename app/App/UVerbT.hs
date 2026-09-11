{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App.UVerbT (UVerbT (..), runUVerbT, ErrorBody (..)) where

-- import App.Errors
import Control.Monad.Except
import Control.Monad.Writer.Strict (MonadIO (liftIO))
import Data.Aeson
import Data.OpenApi
import Data.SOP.BasicFunctors (I (..))
import Data.Text
import Effectful (Eff, IOE, (:>))
import GHC.Generics
import Servant (IsMember, Union)

-- import Servant.API.Status qualified
import Servant.API.UVerb (inject)

newtype UVerbT xs es a = UVerbT {unUVerbT :: ExceptT (Union xs) (Eff es) a}
  deriving newtype (Functor, Applicative, Monad, MonadError (Union xs))

-- instance MonadError e m => MonadError e (UVerbT xs m) where
--   throwError = lift . throwError
--   catchError (UVerbT act) h = UVerbT $ ExceptT $ runExceptT act `catchError` (runExceptT . unUVerbT . h)

instance IOE :> es => MonadIO (UVerbT xs es) where
  liftIO = UVerbT . liftIO

runUVerbT ::
  forall sr xs es a.
  IsMember sr xs => (a -> sr) -> UVerbT xs es a -> Eff es (Union xs)
runUVerbT sr (UVerbT act) = do
  res <- runExceptT act
  pure $ either id (inject . I . sr) res

-- <$> runExceptT act

data ErrorBody = ErrorBody {error :: Text, message :: Text}
  deriving
    ( Show
    , Generic
    , ToJSON
    , ToSchema
    )

-- mapAppError :: AppError status -> ErrorBody
-- mapAppError = \case
--   InvalidUserName n -> ErrorBody "invalid_name" $ "Name \"" <> pack n <> "\" is not valid"
--   UserNotFound uid -> ErrorBody "user_not_found" $ "User with id " <> (pack . Prelude.show $ uid) <> " not found"
--   DuplicatedUser n -> ErrorBody "duplicated_user" $ "Name \"" <> pack n <> "\" already exists"
--   Denied -> ErrorBody "access_denied" "access denied"
--   BadCredentials -> ErrorBody "bad_credentials" "bad credentials"
--   TokenCreationFail -> ErrorBody "token_creation_fail" "token creation fail"

-- throwUVerb ::
--   forall s xs es.
--   (Servant.API.Status.KnownStatus s, IsMember (WithStatus s ErrorBody) xs) =>
--   AppError s -> UVerbT xs es ()
-- throwUVerb e = UVerbT . ExceptT . fmap Left . respond $ WithStatus @s $ mapAppError e
