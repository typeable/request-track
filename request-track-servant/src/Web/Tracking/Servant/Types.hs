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
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Internal

-- | Types to use in you API description
data RequestTracking

-- | Same as above, except no argument is passed to a handler.
data RequestTracking_

newtype ReqtrackingHandler r o = ReqtrackingHandler { unReqtrackingHandler :: r -> Handler o
                                                    } deriving (Functor)

mkReqtrackingHandler :: (r -> Handler o) -> ReqtrackingHandler r o
mkReqtrackingHandler = ReqtrackingHandler

instance forall (api :: Type) context.
         ( HasServer api context
         , HasContextEntry context (ReqtrackingHandler Request (Maybe ReqtrackInfo))
         ) =>  HasServer (RequestTracking :> api) context where
  type ServerT (RequestTracking :> api) m = Maybe ReqtrackInfo -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ context subserver = route (Proxy :: Proxy api) context (subserver `addHeaderCheck` withRequest trackingCheck)
    where trackingCheck :: Request -> DelayedIO (Maybe ReqtrackInfo)
          trackingCheck = either delayedFailFatal return <=<  liftIO . runHandler . trackingHandler
          trackingHandler :: Request -> Handler (Maybe ReqtrackInfo)
          trackingHandler = let ReqtrackingHandler h = getContextEntry context
                            in h

instance forall (api :: Type) context.
         ( HasServer api context
         , HasContextEntry context (ReqtrackingHandler Request (Maybe ReqtrackInfo))
         ) =>  HasServer (RequestTracking_ :> api) context where
  type ServerT (RequestTracking_ :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route _ context subserver = route (Proxy :: Proxy api) context (fmap const subserver `addHeaderCheck` withRequest trackingCheck)
    where trackingCheck :: Request -> DelayedIO ()
          trackingCheck = either delayedFailFatal (const $ return ())<=<  liftIO . runHandler . trackingHandler
          trackingHandler :: Request -> Handler (Maybe ReqtrackInfo)
          trackingHandler = let ReqtrackingHandler h = getContextEntry context
                            in h


type ReqtrackHeaders = '[ Header RequestIdHdr ByteString
                        , Header RequestLevelHdr Word
                        , Header RequestSourceHdr ByteString
                        ]
