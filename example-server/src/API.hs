{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module API where

import Data.Proxy
import Servant.API
import Web.Tracking.Servant

type ExampleAPI = RequestTracking  :> "echo"  :> Get '[JSON] String
             :<|> RequestTracking_ :> "relay" :> Get '[JSON] String

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy
