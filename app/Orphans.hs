{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans () where

import Control.Lens ((&), (<>~))
import Data.OpenApi
import Servant
import Servant.OpenApi

instance HasOpenApi api => HasOpenApi (BasicAuth realm usr :> api) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy api)
      & components
        . securitySchemes
        <>~ SecurityDefinitions
          [("BasicAuth", basicAuthSchema)]
      & allOperations
        . security
        <>~ [SecurityRequirement [("BasicAuth", [])]]
    where
      basicAuthSchema =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
          , _securitySchemeDescription = Just "Basic access authentication"
          }