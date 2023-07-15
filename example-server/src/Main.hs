{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.ThreadLocal
import Control.Monad.IO.Class
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Web.Tracking
import Web.Tracking.Servant

type ExampleAPI = RequestTracking :> "echo" :> Get '[JSON] String
             :<|> RequestTracking :> "relay" :> Get '[JSON] String

echoHandler :: Maybe ReqtrackInfo -> Handler String
echoHandler _ = pure "PONG"

relayHandler :: Maybe ReqtrackInfo -> Handler String
relayHandler _ = pure "RELAY"

rqHandler :: Request -> Handler (Maybe ReqtrackInfo)
rqHandler req | Just info <- parseTrackingHeaders req =
                  liftIO $ do putStrLn (unlines [ "Request tracking headers found!"
                                                , "Request Id: " <> show (requestId info)
                                                , "Request Source: " <> show (requestSource info)
                                                , "Request Level: " <> show (requestLevel info)
                                                ])
                              return (Just info)
              | otherwise = liftIO $ Nothing <$ putStrLn "Request tracking headers not found!"

server :: Server ExampleAPI
server = echoHandler :<|> relayHandler

main :: IO ()
main = do tls <- newThreadLocalWithGC 60
          run 1337 $ trackingMiddleware (`insertThreadLocal` tls)
                   $ serveWithContext (Proxy :: Proxy ExampleAPI) ctx server
  where ctx = mkReqtrackingHandler rqHandler :. EmptyContext
