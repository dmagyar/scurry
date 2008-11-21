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

    forever $ do
        mv' <- takeMVar mv
        case mv' of
            HB   -> return ()
            CR m -> msgHandler sr m

heartBeatThread :: MVar MM -> IO ()
heartBeatThread mv = forever $ do
    putStrLn $ "HB"
    threadDelay (msToS 5)
    putMVar mv HB

chanReadThread :: MVar MM -> ConMgrChan -> IO ()
chanReadThread mv cmc = forever $ let rd = (atomically $ readTChan cmc)
                                      pt = (putMVar mv) . CR
                                  in rd >>= pt

msgHandler :: StateRef -> (EndPoint,ScurryMsg) -> IO ()                                  
msgHandler sr (ep,sm) = do
    putStrLn $ "Msg"
    case sm of
        SKeepAlive -> return ()
        bad -> error $ "Software Design Error: msgHandler can't use " ++ (show bad)
