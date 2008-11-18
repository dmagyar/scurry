module Scurry.Comm.SockSource (
sockSourceThread
) where

import Control.Monad (forever)
import Data.Binary
import Data.List (find,nubBy)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import Data.IORef
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
import Scurry.Types

sockSourceThread :: TapDesc -> Socket -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockSourceThread tap sock ssRef chan = forever $ do
    (addr,msg) <- sockReader sock
    routeInfo tap ssRef chan (addr,sockDecode msg)
    return ()
    
sockReader :: Socket -> IO (SockAddr,BSS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (addr,msg)

routeInfo :: TapDesc -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> (SockAddr,ScurryMsg) -> IO ()
routeInfo tap ssRef chan (srcAddr,msg) = do

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
         SJoin mac        -> atomicUpdatePeers (Just mac) srcAddr >> joinReply >> notify srcAddr
         SJoinReply mac p -> do putStrLn $ "Got peer list: " ++ (show p)
                                atomicUpdatePeers (Just mac) srcAddr
                                mapM_ (atomicUpdatePeers Nothing) p
         SKeepAlive       -> return ()
         SNotifyPeer np   -> gotNotify np
         SRequestPeer     -> putStrLn "Error: SRequestPeer not supported"
         SPing pid        -> writeChan (DestSingle srcAddr) (SEcho pid)
         SEcho eid        -> putStrLn $ "Echo: " ++ (show eid) ++ (show $ srcAddr)
         SUnknown         -> putStrLn $ "Error: Received an unknown message tag."
    where atomicUpdatePeers mac sa = atomicModifyIORef ssRef (addPeer mac sa)
          addPeer mac sa (ScurryState ps m) = let nubber (_,a) (_,b) = a == b
                                                  peers = nubBy nubber $ (mac,sa) : ps
                                              in (ScurryState peers m,())
          writeChan d m = atomically $ writeTChan chan (d,m)
          joinReply = do
            (ScurryState peers (_,mymac)) <- readIORef ssRef
            -- TODO: The other side should also verify that it doesn't add itself to the peer list
            writeChan (DestSingle srcAddr) $ SJoinReply mymac $ filter (/= srcAddr) $ map (\(_,p) -> p) peers
          notify p = do
            (ScurryState peers _) <- readIORef ssRef
            writeChan (DestList $ filter (/= p) $ map (\(_,x) -> x) peers) (SNotifyPeer p)
          gotNotify p = do
            (ScurryState peers _) <- readIORef ssRef
            let e = find (\(_,x) -> x == p) peers
            case e of
                 Nothing -> atomicUpdatePeers Nothing p
                 (Just _) -> putStrLn $ "Already have peer " ++ (show p)

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])

