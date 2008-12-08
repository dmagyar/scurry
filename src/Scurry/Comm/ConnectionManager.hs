module Scurry.Comm.ConnectionManager (
    conMgrThread,
) where

import Data.List (find)
import qualified Data.Map as M
import Control.Monad (forever)
import Control.Concurrent.STM
import GHC.Conc
import Data.Time

import Scurry.State
import Scurry.Util
import Scurry.Types.Threads
import Scurry.Types.Network
import Scurry.Comm.Message
import Scurry.Comm.Util

-- | Connection Manager Thread State
data CMTState = CMTState {
        kaStatus :: EPKAMap
    } deriving (Show)

-- | End Point to Keep Alive time mapping
type EPKAMap = M.Map EndPoint EPStatus

-- | The maximum number of attempts we'll try and 
-- establish a connection.
maxEstablishAttempts :: Int
maxEstablishAttempts = 5

-- | The amount of time we will allow a peer to go
-- without a KeepAlive before we drop the connection.
-- In seconds.
staleConnection :: Int
staleConnection = 60

-- | A status type that determines the state specific
-- peers are in.
--  - EPUnestablished holds the number of attempts to connect
--  - EPEstablished holds the time since the last KeepAlive
data EPStatus = EPUnestablished Int
              | EPEstablished UTCTime
    deriving (Show)

-- | (M)anager (M)essage is either a (H)eart (B)eat or a (C)hannel (R)ead
data MM = HB
        | CR (EndPoint,ScurryMsg)

-- | Connection Manager Thread
conMgrThread :: StateRef -> SockWriterChan -> ConMgrChan -> IO ()
conMgrThread sr swc cmc = do
    mv <- newEmptyMVar
    hb <- forkIO $ heartBeatThread mv 
    cr <- forkIO $ chanReadThread mv cmc

    labelThread hb "CMT's Heart Beat Thread"
    labelThread cr "CMT's Channel Reader Thread"

    manage mv (CMTState M.empty) >> return ()

    where
        -- | cmts is the conMgrThread state--an internal piece
        -- of state used to keep track of things unique to the
        -- manage function below.
        manage :: MVar MM -> CMTState -> IO ()
        manage mv cmts = do
            mv' <- takeMVar mv
            cmts' <- case mv' of
                          HB   -> return cmts
                          CR m -> msgHandler sr swc cmts m
            cmts'' <- checkConnections sr cmts'            
            manage mv cmts'' -- Recursive call


-- | The Heart Beat Thread's purpose is to wake up the Connection
-- Manager and have it check everything for sanity and make sure
-- any stale connections are cleaned out.
heartBeatThread :: MVar MM -> IO ()
heartBeatThread mv = forever $ do
    threadDelay (msToS 5)
    putMVar mv HB

-- | The Channel Reader Thread's purpose is to pull an item off
-- from the TChan and alert the Connection Manager when an item
-- is ready for processing.
chanReadThread :: MVar MM -> ConMgrChan -> IO ()
chanReadThread mv cmc = forever $ let rd = (atomically $ readTChan cmc)
                                      pt = (putMVar mv) . CR
                                  in rd >>= pt
-- | Connection Checker
checkConnections :: StateRef -> CMTState -> IO CMTState
checkConnections sr cmts = do
    ct <- getCurrentTime
    return cmts

msgHandler :: StateRef -> SockWriterChan -> CMTState -> (EndPoint,ScurryMsg) -> IO CMTState
msgHandler sr swc cmts (ep,sm) = do
    case sm of
        SKeepAlive       -> r_SKeepAlive
        SJoin mac        -> r_SJoin mac
        SJoinReply mac p -> r_SJoinReply mac p
        SNotifyPeer np   -> r_SNotifyPeer np
        SRequestPeer     -> r_SRequestPeer

        bad -> r_bad bad

    where
        r_SKeepAlive = return cmts
        
        r_SJoin mac = do
            addPeer sr ((Just mac),ep)
            joinReply
            joinNotify
            return cmts

        r_SJoinReply mac p = do
            addPeer sr ((Just mac),ep)
            mapM_ ((addPeer sr) . ((,) Nothing)) p
            return cmts

        r_SNotifyPeer np = do
            gotNotify np
            return cmts

        r_SRequestPeer = do
            putStrLn "Error: SRequestPeer not supported"
            return cmts

        r_bad bad = error $ "Software Design Error: msgHandler can't use " ++ (show bad)

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
