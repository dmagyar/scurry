module Scurry.KeepAlive (
    keepAliveThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.IORef
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State

keepAliveThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
keepAliveThread ssRef chan = forever $ do
    threadDelay 100
