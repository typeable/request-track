module TLS where

import Control.Concurrent.ThreadLocal
import Web.Tracking
import System.IO.Unsafe (unsafePerformIO)

storage :: ThreadLocal ReqtrackInfo
storage = unsafePerformIO (newThreadLocalWithGC 60)
{-# NOINLINE storage #-}
