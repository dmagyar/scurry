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

msToS :: Int -> Int
msToS = (* 1000000)

keepAliveThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
keepAliveThread ssRef chan = forever $ do
    putStrLn "Keep Alive"
    (ScurryState peers) <- readIORef ssRef
    atomically $ writeTChan chan (DestList peers,SKeepAlive)
    threadDelay (msToS 10)
