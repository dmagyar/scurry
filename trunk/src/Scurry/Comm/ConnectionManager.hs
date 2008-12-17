module Scurry.Comm.ConnectionManager (
    conMgrThread,
) where

import qualified Data.Map as M
import Control.Monad (forever)
import Control.Concurrent.STM
import GHC.Conc
import Data.Time

import Scurry.State
import Scurry.Peer
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
staleConnection :: NominalDiffTime
staleConnection = 60 -- seconds

-- | A status type that determines the state specific
-- peers are in.
--  - EPUnestablished holds the number of attempts to connect
--  - EPEstablished holds the time since the last KeepAlive
data EPStatus = EPUnestablished Int
              | EPEstablished UTCTime
    deriving (Show)

-- | An EPStatus representing a new, unestablished peer.
freshEPStatus :: EPStatus
freshEPStatus = EPUnestablished 0

-- | (M)anager (M)essage is either a (H)eart (B)eat or a (C)hannel (R)ead
data MM = HB
        | CR (EndPoint,ScurryMsg)

-- | Connection Manager Thread
conMgrThread :: StateRef -> SockWriterChan -> ConMgrChan -> [EndPoint] -> IO ()
conMgrThread sr swc cmc eps = do
    mv <- newEmptyMVar
    hb <- forkIO $ heartBeatThread mv 
    cr <- forkIO $ chanReadThread mv cmc

    labelThread hb "CMT's Heart Beat Thread"
    labelThread cr "CMT's Channel Reader Thread"

    -- Impersonate the HeartBeat thread to kick things off immediately
    -- after 'manage' begins.
    putMVar mv HB

    -- Add all the tracker end points to the initial CMTState variable.
    manage mv $ CMTState (M.fromList $ zip eps (repeat freshEPStatus))

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

            (cleanConnections sr cmts') >>= (manageConnections sr swc) >>= (manage mv)

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
-- | Connection Cleaner:
--   - Make sure that the connections are current (not stale)
--   - Make sure that the connections are established
cleanConnections :: StateRef -> CMTState -> IO CMTState
cleanConnections sr cmts = do
    ct <- getCurrentTime

    -- 1. Grab all the EndPoints (ps')
    -- 2. Check which of them need to be removed (bad)
    -- 3. Make a map we can run a difference on (bad')
    -- 4. Make a final map with all the good peers (good)
    let ps = check_cmts ct
        bad = filter (\(_,v) -> v) ps
        bad' = M.fromList $ map (\(e,_) -> (e,Nothing)) bad
        good = M.differenceWithKey (\_ _ _ -> Nothing) (kaStatus cmts) bad'

    mapM_ (delPeer sr . fst) bad
    return $ cmts { kaStatus = good }

    where
        check_cmts ct = let cs = M.toList (kaStatus cmts)
                            f (ep,eps) = (ep,hdl_eps eps ct)
                        in  map f cs

        hdl_eps e ct = case e of
                            EPUnestablished ue -> if ue > maxEstablishAttempts
                                                     then True
                                                     else False
                            EPEstablished   es -> if (ct `diffUTCTime` es) > staleConnection
                                                     then True
                                                     else False

-- | Connection Manager:
--   - Sends out a SJoin message if the peer hasn't been connected yet
manageConnections :: StateRef -> SockWriterChan -> CMTState -> IO CMTState
manageConnections sr swc cmts = do

    rec <- getMyRecord sr

    let l = M.toList (kaStatus cmts)
        w_chan msg = atomically $ writeTChan swc msg
        m t@(ep,s) = case s of
                          -- The peer is not established, send a join request
                          (EPUnestablished x) -> do w_chan $ (DestSingle ep,SJoin (rec { peerEndPoint = ep }))
                                                    return (ep,EPUnestablished (x + 1))
                          -- The peer is fine, don't do anything
                          _ -> return t

    (mapM m l) >>= (return . (\x -> cmts { kaStatus = x }) . M.fromList)

msgHandler :: StateRef -> SockWriterChan -> CMTState -> (EndPoint,ScurryMsg) -> IO CMTState
msgHandler sr swc cmts (ep,sm) = do
    case sm of
        SKeepAlive       -> r_SKeepAlive
        SJoin rec        -> r_SJoin rec
        SJoinReply rec p -> r_SJoinReply rec p
        SNotifyPeer np   -> r_SNotifyPeer np
        SRequestPeer     -> r_SRequestPeer
        SLANProbe        -> r_SLANProbe
        SLANSuggest pn   -> r_SLANSuggest pn

        bad -> r_bad bad

    where
        keepOld _ o = o

        r_SKeepAlive = do
            ct <- getCurrentTime
            return $ cmts { kaStatus = (M.insert ep
                                                 (EPEstablished ct)
                                                 (kaStatus cmts)) }
        
        r_SJoin rec = do
            ct <- getCurrentTime
            addPeer sr (rec { peerEndPoint = ep })
            joinReply -- Send a reply
            joinNotify -- Notify every one else of this join
            lannerCheck ep
            return $ cmts { kaStatus = M.insert ep (EPEstablished ct) (kaStatus cmts) }

        r_SJoinReply rec p = do
            ct <- getCurrentTime
            addPeer sr (rec { peerEndPoint = ep })

            let cmts' = cmts { kaStatus = M.insert ep (EPEstablished ct) (kaStatus cmts) }
                cmts'' = cmts' { kaStatus = foldr (\k m -> M.insertWith keepOld k freshEPStatus m) (kaStatus cmts') p }
            
            lannerCheck ep

            return cmts''

        -- | Some one has informed us of a new peer. Check if we have
        -- it in our list already. 
        r_SNotifyPeer np = do
            let s = kaStatus cmts
                e = M.lookup np s
            case e of
                 Nothing  -> return $ cmts {kaStatus = M.insert np freshEPStatus s}
                 (Just _) -> return cmts -- Either establishing or connected

        r_SLANProbe = return $ cmts { kaStatus = M.insertWith keepOld ep freshEPStatus (kaStatus cmts) }

        r_SLANSuggest port = do
            let bcastAddr = ScurryAddress 0xFFFFFFFF
                dest = DestSingle (EndPoint bcastAddr port)
            atomically $ writeTChan swc (dest,SLANProbe)
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
                e' = map peerEndPoint peers
                p' = filter (/= ep) e'
                m = SJoinReply mymac p'
            atomically $ writeTChan swc (d,m)

        -- | This alerts the other peers on our network that some one
        -- has joined us.
        joinNotify = do
            peers <- getPeers sr
            let d = (DestList $ filter (/= ep) $ map peerEndPoint peers)
                m = (SNotifyPeer ep)
            atomically $ writeTChan swc (d,m)

        -- | Check if the provided address is the same as another in
        -- our peer list (this most likely means they are on the same
        -- LAN). If we find one, we send a SLANSuggest with the 
        -- corresponding port number.
        lannerCheck e@(EndPoint addr _) = do
            peers <- getPeers sr
            
            let lan = map (\(EndPoint _ p) -> p) $ filter (\e'@(EndPoint a _) -> (e' /= e) && (addr == a)) $ map peerEndPoint peers
                dst = DestSingle e
                wrt m = atomically $ writeTChan swc (dst,m)

            putStrLn $ (show lan)

            mapM_ wrt (map SLANSuggest lan)

