{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Domain.Users (UserId (..), User (..)) where

import Data.Aeson
import GHC.Generics

newtype UserId = UserId Int
  deriving stock (Eq, Show, Generic)
  deriving newtype
    ( ToJSON
    )

data User = User {uid :: UserId, name :: String}
  deriving
    ( Show
    , Generic
    , ToJSON
    )
