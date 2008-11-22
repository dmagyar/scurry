module Scurry.Comm.ConnectionManager (
    conMgrThread,
) where

import Data.List (find)
import Control.Monad (forever)
import Control.Concurrent.STM
import GHC.Conc

import Scurry.State
import Scurry.Util
import Scurry.Types.Threads
import Scurry.Types.Network
import Scurry.Comm.Message
import Scurry.Comm.Util

-- | (M)anager (M)essage is either a (H)eart (B)eat or a (C)hannel (R)ead
data MM = HB
        | CR (EndPoint,ScurryMsg)

conMgrThread :: StateRef -> SockWriterChan -> ConMgrChan -> IO ()
conMgrThread sr swc cmc = do
    mv <- newEmptyMVar
    hb <- forkIO $ heartBeatThread mv 
    cr <- forkIO $ chanReadThread mv cmc

    manage mv >> return ()

    where
        manage mv = do
            mv' <- takeMVar mv
            case mv' of
                HB   -> hbHandler sr
                CR m -> msgHandler sr swc m
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

msgHandler :: StateRef -> SockWriterChan -> (EndPoint,ScurryMsg) -> IO ()                                  
msgHandler sr swc (ep,sm) = do
    case sm of
        SKeepAlive       -> return ()
        SJoin mac        -> addPeer sr ((Just mac),ep) >>
                            joinReply >> joinNotify
        SJoinReply mac p -> addPeer sr ((Just mac),ep) >>
                            mapM_ ((addPeer sr) . ((,) Nothing)) p
        SNotifyPeer np   -> gotNotify np

        bad -> error $ "Software Design Error: msgHandler can't use " ++ (show bad)

    where
        -- | When we get a SJoin message, we add the new peer to our
        -- own peer list (if we don't have them recorded yet), inform
        -- them of our MAC address and the peers we know about, and
        -- then tell every one else we know about the new peer.
        joinReply = do
            (ScurryState peers _ mymac) <- getState sr        
            let d = (DestSingle ep)
                e' = map (\(_,p) -> p) peers
                p' = filter (/= ep) e'
                m = SJoinReply mymac p'
            atomically $ writeTChan swc (d,m)

        -- | This alerts the other peers on our network that some one
        -- has joined us.
        joinNotify = do
            peers <- getPeers sr
            let d = (DestList $ filter (/= ep) $ map (\(_,x) -> x) peers)
                m = (SNotifyPeer ep)
            atomically $ writeTChan swc (d,m)

        -- | Some one has informed us of a new peer. Check if we have
        -- it in our list already. If we don't mark its MAC address
        -- as Nothing in the peer list so that the KeepAlive will send
        -- a SJoin message instead of a regular SKeepAlive to that 
        -- peer.
        gotNotify p = do
            peers <- getPeers sr
            let e = find (\(_,x) -> x == p) peers
            case e of
                 Nothing -> addPeer sr (Nothing,p)
                 (Just _) -> putStrLn $ "Already have peer " ++ (show p)
