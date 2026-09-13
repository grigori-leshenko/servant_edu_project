module App.Errors (AppError (..)) where

import App.UsersE (UsersError)

data AppError where
  UsersError :: UsersError -> AppError
  Denied :: AppError
  BadCredentials :: AppError
  TokenCreationFail :: AppError
deriving instance Show AppError
