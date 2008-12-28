module Scurry.KeepAlive (
    keepAliveThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util

import Scurry.State
import Scurry.Peer

import Scurry.Types.Threads

import Scurry.Util

keepAliveThread :: StateRef -> SockWriterChan -> IO ()
keepAliveThread sr chan = forever $ do
    peers <- getPeers sr
    mapM_ messenger peers
    threadDelay (sToMs 10)
    where sendMsg dest msg = atomically $ writeTChan chan (dest,msg)
          messenger pr = sendMsg (DestSingle (peerEndPoint pr)) SKeepAlive
          
            {-
            case mac of
                 Nothing  -> do mymac <- getMAC sr
                                sendMsg (DestSingle addr) (SJoin mymac)
                 (Just _) -> sendMsg (DestSingle addr) (SKeepAlive)
                 -}
