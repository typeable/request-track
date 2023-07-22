{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Concurrent.ThreadLocal
import Control.Monad.IO.Class
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Web.Tracking
import Web.Tracking.Servant

type instance TrackingData RequestTracking = ()

type ExampleAPI = RequestTracking :> "echo" :> Get '[JSON] String
             :<|> RequestTracking_ :> "relay" :> Get '[JSON] String

echoHandler :: () -> Handler String
echoHandler _ = pure "PONG"

relayHandler :: Handler String
relayHandler = pure "RELAY"

rqHandler :: Maybe ReqtrackInfo -> Handler ()
rqHandler (Just info) = liftIO $ putStrLn (unlines [ "Request tracking headers found!"
                                                   , "Request Id: " <> show (requestId info)
                                                   , "Request Source: " <> show (requestSource info)
                                                   , "Request Level: " <> show (requestLevel info)
                                                   ])
rqHandler Nothing = liftIO $ putStrLn "Request tracking headers not found!"

server :: Server ExampleAPI
server = echoHandler :<|> relayHandler

main :: IO ()
main = do tls <- newThreadLocalWithGC 60
          run 1337 $ trackingMiddleware (`insertThreadLocal` tls)
                   $ serveWithContext (Proxy :: Proxy ExampleAPI) ctx server
  where ctx = mkReqtrackingHandler rqHandler :. mkReqtrackingHandler rqHandler :. EmptyContext
