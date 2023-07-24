{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import API
import Server
import TLS

import Control.Concurrent.ThreadLocal
import Network.Wai.Handler.Warp
import Servant.Server
import Web.Tracking hiding (addTrackingHeaders)
import Web.Tracking.Servant

main :: IO ()
main = run 8081 $ trackingMiddleware (`insertThreadLocal` storage)
                $ serveWithContext exampleAPI ctx server
  where ctx =  mkReqtrackingHandler rqHandler
            :. mkReqtrackingHandler rqHandler_
            :. EmptyContext
