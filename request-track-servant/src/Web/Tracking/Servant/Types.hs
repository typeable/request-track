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

-- | Types to use in you API description
data RequestTracking

type family TrackingData a

-- | Same as above, except no argument is passed to a handler.
data RequestTracking_

newtype ReqtrackingHandler r o = ReqtrackingHandler { unReqtrackingHandler :: r -> Handler o
                                                    } deriving (Functor)

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


type ReqtrackHeaders = '[ Header RequestIdHdr ByteString
                        , Header RequestLevelHdr Word
                        , Header RequestSourceHdr ByteString
                        ]
