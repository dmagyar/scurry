module Scurry.Comm.ConnectionManager (
    conMgrThread,
) where

import Scurry.State
import Scurry.Types.Threads
import Control.Monad (forever)
import Control.Concurrent.STM
import GHC.Conc

import Scurry.Util
import Scurry.Types.Network
import Scurry.Comm.Message

-- | (M)anager (M)essage is either a (H)eart (B)eat or a (C)hannel (R)ead
data MM = HB
        | CR (EndPoint,ScurryMsg)

conMgrThread :: StateRef -> SockWriterChan -> ConMgrChan -> IO ()
conMgrThread sr swc cmc = do
    mv <- newEmptyMVar
    hb <- forkIO $ heartBeatThread mv 
    cr <- forkIO $ chanReadThread mv cmc

    manage mv >> return ()

    where manage mv = do
              mv' <- takeMVar mv
              case mv' of
                   HB   -> hbHandler sr
                   CR m -> msgHandler sr m
              manage mv -- Recursive call


heartBeatThread :: MVar MM -> IO ()
heartBeatThread mv = forever $ do
    threadDelay (msToS 5)
    putMVar mv HB

chanReadThread :: MVar MM -> ConMgrChan -> IO ()
chanReadThread mv cmc = forever $ let rd = (atomically $ readTChan cmc)
                                      pt = (putMVar mv) . CR
                                  in rd >>= pt

hbHandler :: StateRef -> IO ()
hbHandler sr = return ()

msgHandler :: StateRef -> (EndPoint,ScurryMsg) -> IO ()                                  
msgHandler sr (ep,sm) = do
    case sm of
        SKeepAlive -> return ()
        bad -> error $ "Software Design Error: msgHandler can't use " ++ (show bad)
