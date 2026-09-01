{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Errors (AppError (..)) where

import GHC.TypeLits (Nat)
import Users (UserId)

data AppError (status :: Nat) where
  UserNotFound :: UserId -> AppError 404
  InvalidUserName :: String -> AppError 400
  DuplicatedUser :: String -> AppError 409
deriving instance Show (AppError status)
