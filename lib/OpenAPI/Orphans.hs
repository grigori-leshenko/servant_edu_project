{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI.Orphans () where

import Control.Lens ((&), (<>~))
import Data.OpenApi
import Servant
import Servant.Auth.Server
import Servant.OpenApi

instance HasOpenApi api => HasOpenApi (Auth '[JWT] usr :> api) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy api)
      & components
        . securitySchemes
        <>~ SecurityDefinitions
          [("JWT", bearerJWTScheme)]
      & allOperations
        . security
        <>~ [SecurityRequirement [("BasicAuth", [])]]
    where
      bearerJWTScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
          , _securitySchemeDescription = Just "JWT via Authorisation: Bearer <token>"
          }