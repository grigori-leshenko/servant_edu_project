module App.Errors (AppError (..), AppError' (..)) where

import App.Users (UserId)
import GHC.TypeLits (Nat)

data AppError (status :: Nat) where
  UserNotFound :: UserId -> AppError 404
  InvalidUserName :: String -> AppError 400
  DuplicatedUser :: String -> AppError 409
  Denied :: AppError 403
  BadCredentials :: AppError 401
  TokenCreationFail :: AppError 401
deriving instance Show (AppError status)

data AppError' where
  UserNotFound' :: UserId -> AppError'
  InvalidUserName' :: String -> AppError'
  DuplicatedUser' :: String -> AppError'
  Denied' :: AppError'
  BadCredentials' :: AppError'
  TokenCreationFail' :: AppError'
deriving instance Show AppError'
