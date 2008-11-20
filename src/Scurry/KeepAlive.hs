module Scurry.KeepAlive (
    keepAliveThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util

import Scurry.State

import Scurry.Types.Threads

msToS :: Int -> Int
msToS = (* 1000000)

keepAliveThread :: StateRef -> SockWriterChan -> IO ()
keepAliveThread sr chan = forever $ do
    peers <- getPeers sr
    mapM messenger peers
    threadDelay (msToS 10)
    where sendMsg dest msg = atomically $ writeTChan chan (dest,msg)
          messenger (mac,addr) = do
            case mac of
                 Nothing  -> do mymac <- getMAC sr
                                sendMsg (DestSingle addr) (SJoin mymac)
                 (Just _) -> sendMsg (DestSingle addr) (SKeepAlive)
