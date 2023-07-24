{-# LANGUAGE OverloadedStrings #-}
module Client where

import API
import TLS

import Control.Concurrent.ThreadLocal
import Control.Monad.Catch
import Network.HTTP.Client
import Servant.API
import Servant.Client
import Web.Tracking (incrLevel, replaceSource)
import Web.Tracking.Client
import Web.Tracking.Servant

callRelayM :: ClientM [Char]
callEchoM :: Maybe ReqtrackInfo -> ClientM [Char]
callEchoM :<|> callRelayM = client exampleAPI

url :: BaseUrl
url = BaseUrl { baseUrlScheme = Http
              , baseUrlHost = "localhost"
              , baseUrlPort = 8080
              , baseUrlPath = ""
              }

callRelay :: IO String
callRelay = do mgr <- newManager defaultManagerSettings
               let env = mkClientEnv mgr url
               r <- runClientM callRelayM env
               either throwM return r

modifyRequest :: ThreadLocal ReqtrackInfo -> Request -> IO Request
modifyRequest tls = addTrackingHeaders (fmap f <$> fetchThreadLocal tls)
  where f rti = replaceSource "relay"
              $ incrLevel rti

callEcho_ :: IO String
callEcho_ = do mgr <- newManager defaultManagerSettings { managerModifyRequest = modifyRequest storage }
               let env = mkClientEnv mgr url
               s <- runClientM (callEchoM Nothing) env
               either throwM return s

callEcho :: ReqtrackInfo -> IO ()
callEcho rti = do mgr <- newManager defaultManagerSettings
                  let env = mkClientEnv mgr url
                  s <- runClientM (callEchoM $ Just rti) env
                  print s
