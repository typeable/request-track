{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Tracking.Servant.OpenAPI where

import Web.Tracking.Servant.Types (RequestTracking, RequestTracking_)

import Data.Proxy
import Servant.API
import Servant.OpenApi

instance HasOpenApi api => HasOpenApi (RequestTracking :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)

instance HasOpenApi api => HasOpenApi (RequestTracking_ :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
