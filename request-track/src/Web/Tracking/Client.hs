module Web.Tracking.Client where

import Web.Tracking.Internal

import Network.HTTP.Client

addTrackingHeaders :: IO (Maybe ReqtrackInfo) -> Request -> IO Request
addTrackingHeaders getRti req = f <$> getRti
  where f Nothing = req
        f (Just rti) = req { requestHeaders = mkTrackingHeaders rti ++ removeTrackingHeaders (requestHeaders req)}

