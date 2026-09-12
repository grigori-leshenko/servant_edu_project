module App.Errors (AppError (..), AppError' (..)) where

import App.Users (UserId)

data AppError where
  UserNotFound :: UserId -> AppError
  InvalidUserName :: String -> AppError
  DuplicatedUser :: String -> AppError
  Denied :: AppError
  BadCredentials :: AppError
  TokenCreationFail :: AppError
deriving instance Show AppError

data AppError' where
  UserNotFound' :: UserId -> AppError'
  InvalidUserName' :: String -> AppError'
  DuplicatedUser' :: String -> AppError'
  Denied' :: AppError'
  BadCredentials' :: AppError'
  TokenCreationFail' :: AppError'
deriving instance Show AppError'
