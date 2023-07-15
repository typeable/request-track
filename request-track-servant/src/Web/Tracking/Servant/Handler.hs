module Web.Tracking.Servant.Handler where

import Web.Tracking

import Network.Wai
import Servant.Server

simpleTrackingHandler :: Request -> Handler (Maybe ReqtrackInfo)
simpleTrackingHandler = pure . parseTrackingHeaders
