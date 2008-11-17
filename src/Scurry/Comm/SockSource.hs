module Scurry.Comm.SockSource (
sockSourceThread
) where

import Control.Monad (forever)
import Data.Binary
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
         SJoin mac        -> atomicUpdatePeers (Just mac) srcAddr >> joinReply
         SJoinReply mac p -> do atomicUpdatePeers (Just mac) srcAddr
                                mapM_ (atomicUpdatePeers Nothing) p
         SKeepAlive       -> return ()
         SNotifyPeer _    -> putStrLn "Error: SNotifyPeer not supported"
         SRequestPeer     -> putStrLn "Error: SRequestPeer not supported"
         SPing pid        -> writeChan srcAddr (SEcho pid)
         SEcho eid        -> putStrLn $ "Echo: " ++ (show eid) ++ (show $ srcAddr)
         SUnknown         -> putStrLn "Error: SUnknown not supported"
    where atomicUpdatePeers mac sa = atomicModifyIORef ssRef (updatePeers mac sa)
          updatePeers mac sa (ScurryState ps m) = let peers = (mac,sa) : (filter (\(_,a) -> a /= srcAddr) ps)
                                                  in (ScurryState peers m,())
          writeChan d m = atomically $ writeTChan chan (DestSingle d,m)
          joinReply = do
            (ScurryState peers mymac) <- readIORef ssRef
            -- TODO: The other side should also verify that it doesn't add itself to the peer list
            writeChan srcAddr $ SJoinReply mymac $ filter (/= srcAddr) $ map (\(_,p) -> p) peers

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])

