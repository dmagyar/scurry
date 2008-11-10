module Scurry.KeepAlive (
    keepAliveThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.IORef
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.Types

msToS :: Int -> Int
msToS = (* 1000000)

keepAliveThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
keepAliveThread ssRef chan = forever $ do
    (ScurryState peers _) <- readIORef ssRef
    atomically $ writeTChan chan (DestList peers,SKeepAlive)
    threadDelay (msToS 10)
