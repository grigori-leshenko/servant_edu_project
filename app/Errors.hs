{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Errors (AppError (..)) where

import GHC.TypeLits (Nat)
import Users (UserId)

data AppError (status :: Nat) where
  UserNotFound :: UserId -> AppError 404
  InvalidUserName :: String -> AppError 400
  DuplicatedUser :: String -> AppError 409
  Denied :: AppError 403
  BadCredentials :: AppError 401
  TokenCreationFail :: AppError 401
deriving instance Show (AppError status)
