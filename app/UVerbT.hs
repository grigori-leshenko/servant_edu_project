{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module UVerbT (UVerbT (..), runUVerbT, throwUVerb) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.RWS
import Servant (HasStatus, IsMember, Union, respond)

newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadError e m => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h = UVerbT $ ExceptT $ runExceptT act `catchError` (runExceptT . unUVerbT . h)

runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) =
  either id id
    <$> runExceptT
      (act >>= respond)

throwUVerb :: (Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond
