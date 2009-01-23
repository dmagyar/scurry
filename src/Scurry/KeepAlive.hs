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
import Scurry.Types.Network

import Scurry.Util

isMVarFull :: (MVar a) -> IO Bool
isMVarFull mv = do
    var <- tryTakeMVar mv
    case var of
         (Just v) -> putMVar mv v >> return True
         Nothing  -> return False


keepAliveThread :: StateRef -> SockWriterChan -> (MVar (ScurryAddress, ScurryMask)) -> IO ()
keepAliveThread sr chan tap_mv = forever $ do
    peers <- getPeers sr
    mapM_ messenger peers
    threadDelay (sToMs 10)

    b <- isMVarFull tap_mv

    if b then return ()
         else mapM_ reqaddr peers

    where sendMsg dest msg = atomically $ writeTChan chan (dest,msg)
          messenger pr = sendMsg (DestSingle (peerEndPoint pr)) SKeepAlive
          reqaddr   pr = sendMsg (DestSingle (peerEndPoint pr)) SAddrRequest
          
            {-
            case mac of
                 Nothing  -> do mymac <- getMAC sr
                                sendMsg (DestSingle addr) (SJoin mymac)
                 (Just _) -> sendMsg (DestSingle addr) (SKeepAlive)
                 -}
