{-# LANGUAGE TypeFamilies #-}
module Server where

import API
import Client

import Control.Monad.IO.Class
import Servant.API
import Servant.Server
import Web.Tracking.Servant

type instance TrackingData RequestTracking = Maybe ReqtrackInfo

echoHandler :: Maybe ReqtrackInfo -> Handler String
echoHandler rti = pure ("PONG, " <> show (requestId <$> rti))

relayHandler :: Handler String
relayHandler = liftIO callEcho_

rqHandler :: Maybe ReqtrackInfo -> Handler (Maybe ReqtrackInfo)
rqHandler (Just info) = liftIO (Just info <$ putStrLn (unlines [ "Request tracking headers found!"
                                                               , "Request Id: " <> show (requestId info)
                                                               , "Request Source: " <> show (requestSource info)
                                                               , "Request Level: " <> show (requestLevel info)
                                                               ]))
rqHandler Nothing = liftIO (Nothing <$ putStrLn "Request tracking headers not found!")

rqHandler_ :: Maybe ReqtrackInfo -> Handler ()
rqHandler_ (Just info) = liftIO (putStrLn (unlines [ "Request tracking headers found!"
                                                   , "Request Id: " <> show (requestId info)
                                                   , "Request Source: " <> show (requestSource info)
                                                   , "Request Level: " <> show (requestLevel info)
                                                   ]))
rqHandler_ Nothing = liftIO (putStrLn "Request tracking headers not found!")


server :: Server ExampleAPI
server = echoHandler :<|> relayHandler
