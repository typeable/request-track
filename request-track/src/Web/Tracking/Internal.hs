{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Tracking.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.CaseInsensitive (CI)
import Data.Proxy
import Data.String
import GHC.TypeLits
import Network.HTTP.Types

-- | Request ID header name.
--   Note that this is unofficial but a widespread thing.
--   See here for more details: https://http.dev/x-request-id
type RequestIdHdr = "X-REQUEST-ID"
requestIdHdr :: CI ByteString
requestIdHdr = fromString (symbolVal (Proxy :: Proxy RequestIdHdr))

-- | Request level header.
--   Contains a number. Each sub-request must be an increment of the received value.
type RequestLevelHdr = "X-REQUEST-LEVEL"
requestLevelHdr :: CI ByteString
requestLevelHdr = fromString (symbolVal (Proxy :: Proxy RequestLevelHdr))

-- | Source for outgoing request.
--   Should be the name of the service that makes a request or a subrequest.
type RequestSourceHdr = "X-REQUEST-SOURCE"
requestSourceHdr :: CI ByteString
requestSourceHdr = fromString (symbolVal (Proxy :: Proxy RequestSourceHdr))

data ReqtrackInfo = ReqtrackInfo { requestId :: ByteString
                                 , requestLevel :: Word
                                 , requestSource :: ByteString
                                 } deriving (Eq, Show)

incrLevel :: ReqtrackInfo -> ReqtrackInfo
incrLevel r@ReqtrackInfo{requestLevel} = r { requestLevel = requestLevel + 1 }

replaceSource :: ByteString -> ReqtrackInfo -> ReqtrackInfo
replaceSource s r = r { requestSource = s }

removeTrackingHeaders :: [Header] -> [Header]
removeTrackingHeaders = filter (\(hdr,_) -> hdr `notElem` [requestIdHdr, requestLevelHdr, requestSourceHdr])

mkTrackingHeaders :: ReqtrackInfo -> [Header]
mkTrackingHeaders rti = [ (requestIdHdr, requestId rti)
                        , (requestLevelHdr, ByteString.pack (show (requestLevel rti)))
                        , (requestSourceHdr, requestSource rti)
                        ]
