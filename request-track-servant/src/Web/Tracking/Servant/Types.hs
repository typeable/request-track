{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Tracking.Servant.Types where

import Web.Tracking

import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Kind
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Network.Wai as Wai
import Servant.API
import Servant.Client.Core 
import Servant.Server
import Servant.Server.Internal

-- | Combinator for your Servant API description. Prepend it to your API for greater enjoyment!
--   Example:
--
-- @
--   type MyAPI = RequestTracking :> "ping" :> Get '[JSON] Text
-- @
--
--   In addition to the API combinator, you also need to create a context handler that will
--   handler the tracking data (see 'ReqtrackInfo') and produce desired output, which in turn
--   will be passed to your API handlers.
--   The type of values passed to API handlers is controlled by @type instance TrackingData RequestTracking@,
--   which isn't set by default.
--   The server code should look something like this:
--
-- @
--    data TrackingArg = ...
--
--    type instance TrackingData RequestTracking = TrackingArg
--
--    trackingHandler :: ReqtrackingHandler (Maybe ReqtrackInfo) TrackingArg
--    trackingHandler = mkReqtrackingHandler $ \mrti ->
--                        case mrti of
--                          Just rti -> ... -- Do something with ReqtrackInfo, return TrackingArg
--                          Nothing -> ...
--
-- @
data RequestTracking

-- | See 'RequestTracking'
type family TrackingData a

-- | Same as above, except no argument is passed to a handler.
data RequestTracking_

-- | Servant context handler used for processing 'ReqtrackInfo' before passing the result to endpoint handlers.
--   Use 'mkReqtrackingHandler' in your code instead of the constructor.
newtype ReqtrackingHandler r o = ReqtrackingHandler { runReqtrackingHandler :: r -> Handler o
                                                    } deriving (Functor)

instance Applicative (ReqtrackingHandler r) where
  pure x = ReqtrackingHandler $ \_ -> pure x
  f <*> v = ReqtrackingHandler $ \r -> runReqtrackingHandler f r <*> runReqtrackingHandler v r

instance Monad (ReqtrackingHandler r) where
  return = pure
  f >>= g = ReqtrackingHandler $ \r -> do a <- runReqtrackingHandler f r
                                          runReqtrackingHandler (g a) r

-- | Smart constructor to create 'ReqtrackingHandler'.
mkReqtrackingHandler :: (Maybe ReqtrackInfo -> Handler o) -> ReqtrackingHandler (Maybe ReqtrackInfo) o
mkReqtrackingHandler = ReqtrackingHandler

instance forall (api :: Type) context.
         ( HasServer api context
         , HasContextEntry context (ReqtrackingHandler (Maybe ReqtrackInfo) (TrackingData RequestTracking))
         ) =>  HasServer (RequestTracking :> api) context where
  type ServerT (RequestTracking :> api) m = TrackingData RequestTracking -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ context subserver = route (Proxy :: Proxy api) context (subserver `addHeaderCheck` withRequest trackingCheck)
    where trackingCheck :: Wai.Request -> DelayedIO (TrackingData RequestTracking)
          trackingCheck = either delayedFailFatal return <=<  liftIO . runHandler . trackingHandler
          trackingHandler :: Wai.Request -> Handler (TrackingData RequestTracking)
          trackingHandler = let ReqtrackingHandler h = getContextEntry context
                            in h . parseTrackingHeaders

instance HasClient m api => HasClient m (RequestTracking :> api) where
  type Client m (RequestTracking :> api) = Maybe ReqtrackInfo -> Client m api

  clientWithRoute pm Proxy req mrti = clientWithRoute pm (Proxy :: Proxy api) req'
    where req' | Just rti <- mrti = req { requestHeaders = Seq.fromList (mkTrackingHeaders rti) <> requestHeaders req }
               | otherwise = req

  hoistClientMonad pm Proxy f cl = \rti -> hoistClientMonad pm (Proxy :: Proxy api) f (cl rti)


instance forall (api :: Type) context.
         ( HasServer api context
         , HasContextEntry context (ReqtrackingHandler (Maybe ReqtrackInfo) ())
         ) =>  HasServer (RequestTracking_ :> api) context where
  type ServerT (RequestTracking_ :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route _ context subserver = route (Proxy :: Proxy api) context (fmap const subserver `addHeaderCheck` withRequest trackingCheck)
    where trackingCheck :: Wai.Request -> DelayedIO ()
          trackingCheck = either delayedFailFatal (const $ return ()) <=<  liftIO . runHandler . trackingHandler
          trackingHandler :: Wai.Request -> Handler ()
          trackingHandler = let ReqtrackingHandler h = getContextEntry context
                            in h . parseTrackingHeaders

instance HasClient m api => HasClient m (RequestTracking_ :> api) where
  type Client m (RequestTracking_ :> api) = Client m api

  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm Proxy f cl = hoistClientMonad pm (Proxy :: Proxy api) f cl


-- | Headers list to use with 'Headers'. Currently unused.
type ReqtrackHeaders = '[ Header RequestIdHdr ByteString
                        , Header RequestLevelHdr Word
                        , Header RequestSourceHdr ByteString
                        ]
