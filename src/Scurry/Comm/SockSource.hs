module Scurry.Comm.SockSource (
sockSourceThread
) where

import Control.Monad (forever)
import Data.Binary
import Data.List (find)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import System.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.STM.TChan
import GHC.Conc

import Scurry.TapConfig
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State
import Scurry.Types.Network
import Scurry.Types.TAP
import Scurry.Types.Threads

sockSourceThread :: TapDesc -> Socket -> StateRef -> SockWriterChan -> ConMgrChan -> IO ()
sockSourceThread tap sock sr swchan cmchan = forever $ do
    (addr,msg) <- sockReader sock
    routeInfo tap sr swchan cmchan (addr,sockDecode msg)
    return ()
    
sockReader :: Socket -> IO (EndPoint,BSS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (saToEp addr,msg)

routeInfo :: TapDesc -> StateRef -> SockWriterChan -> ConMgrChan -> (EndPoint,ScurryMsg) -> IO ()
routeInfo tap sr swchan cmchan (srcAddr,msg) = do

    {-
    case msg of
         SFrame _       -> putStrLn $ (show srcAddr) ++ " -> " ++ "SFrame"
         SJoin  _       -> putStrLn $ (show srcAddr) ++ " -> " ++ "SJoin"
         SJoinReply _ _ -> putStrLn $ (show srcAddr) ++ " -> " ++ "SJoinReply"
         SKeepAlive     -> putStrLn $ (show srcAddr) ++ " -> " ++ "SKeepAlive"
         _ -> return ()
    -}

    case msg of
         SFrame (_,frame) -> write_tap tap frame
         SJoin mac        -> addPeer sr ((Just mac),srcAddr) >> joinReply >> notify srcAddr
         SJoinReply mac p -> do putStrLn $ "Got peer list: " ++ (show p)
                                addPeer sr ((Just mac),srcAddr)
                                mapM_ ((addPeer sr) . ((,) Nothing)) p
         SKeepAlive       -> return ()
         SNotifyPeer np   -> gotNotify np
         SRequestPeer     -> putStrLn "Error: SRequestPeer not supported"
         SPing pid        -> writeChan (DestSingle srcAddr) (SEcho pid)
         SEcho eid        -> putStrLn $ "Echo: " ++ (show eid) ++ (show $ srcAddr)
         SUnknown         -> putStrLn $ "Error: Received an unknown message tag."
    where writeChan d m = atomically $ writeTChan swchan (d,m)
          joinReply = do
            (ScurryState peers _ mymac) <- getState sr
            -- TODO: The other side should also verify that it doesn't add itself to the peer list
            writeChan (DestSingle srcAddr) $ SJoinReply mymac $ filter (/= srcAddr) $ map (\(_,p) -> p) peers
          notify p = do
            peers <- getPeers sr
            writeChan (DestList $ filter (/= p) $ map (\(_,x) -> x) peers) (SNotifyPeer p)
          gotNotify p = do
            peers <- getPeers sr
            let e = find (\(_,x) -> x == p) peers
            case e of
                 Nothing -> addPeer sr (Nothing,p)
                 (Just _) -> putStrLn $ "Already have peer " ++ (show p)

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])

