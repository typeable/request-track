module Web.Tracking.Headers where

import Web.Tracking.Internal

import qualified Data.ByteString.Char8 as ByteString
import Network.HTTP.Types
import Network.Wai
import Text.Read (readMaybe)

parseTrackingHeaders :: Request -> Maybe ReqtrackInfo
parseTrackingHeaders req = ReqtrackInfo <$> reqId
                                        <*> reqLevel
                                        <*> reqSource
  where headers = requestHeaders req
        reqId = lookup requestIdHdr headers
        reqLevel = readMaybe . ByteString.unpack =<< lookup requestLevelHdr headers
        reqSource = lookup requestSourceHdr headers

addTrackingHeaders :: ReqtrackInfo -> Response -> Response
addTrackingHeaders rti = mapResponseHeaders f
  where headers :: ResponseHeaders
        headers = [ (requestIdHdr, requestId rti)
                  , (requestLevelHdr, ByteString.pack (show (requestLevel rti)))
                  , (requestSourceHdr, requestSource rti)
                  ]
        filterHdrs = filter (\(hdr,_) -> hdr `notElem` [requestIdHdr, requestLevelHdr, requestSourceHdr])
        f hdrs = headers ++  filterHdrs hdrs

trackingMiddleware :: (ReqtrackInfo -> IO ()) -> Middleware
trackingMiddleware f app req resp
  | Just rti <- parseTrackingHeaders req = do f rti
                                              app req (resp . addTrackingHeaders rti)
  | otherwise = app req resp
