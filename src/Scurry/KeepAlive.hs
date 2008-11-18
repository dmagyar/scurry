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
    mapM messenger peers
    threadDelay (msToS 10)
    where sendMsg dest msg = atomically $ writeTChan chan (dest,msg)
          messenger (mac,addr) = do
            case mac of
                 Nothing  -> do (ScurryState _ (_,mymac)) <- readIORef ssRef
                                sendMsg (DestSingle addr) (SJoin mymac)
                 (Just _) -> sendMsg (DestSingle addr) (SKeepAlive)
