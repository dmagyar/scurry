module Scurry.Comm.SockSource (
    sockSourceThread,
) where

import Control.Monad (forever)
import Data.Binary
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import System.IO
import Network.Socket.ByteString (recvFrom)
import Network.Socket (Socket)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.STM.TChan
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State
import Scurry.Types.Network
import Scurry.Types.Threads

sockSourceThread :: TapWriterChan -> Socket -> StateRef -> SockWriterChan -> ConMgrChan -> (MVar (ScurryAddress, ScurryMask)) -> IO ()
sockSourceThread tap sock sr swchan cmchan tap_mv = forever $ do
    (msg,addr) <- recvFrom sock readLength

    routeInfo tap sr swchan cmchan (saToEp addr,sockDecode msg) tap_mv
    return ()

    where sockDecode = decode  . BS.fromChunks . return
    
routeInfo :: TapWriterChan -> StateRef -> SockWriterChan -> ConMgrChan -> (EndPoint,ScurryMsg) -> (MVar (ScurryAddress, ScurryMask)) -> IO ()
routeInfo tap _ swchan cmchan (srcAddr,msg) tap_mv =
    case msg of
         SFrame frame   -> atomically $ writeTChan tap frame
         SKeepAlive     -> fwd
         SJoin _        -> fwd
         SJoinReply _ _ -> fwd
         SNotifyPeer _  -> fwd
         SRequestPeer   -> fwd
         SLANProbe      -> fwd
         SLANSuggest _  -> fwd
         SPing pid      -> sckWrtWrite (DestSingle srcAddr) (SEcho pid)
         SEcho eid      -> putStrLn $ "Echo: " ++ show eid ++ show srcAddr
         SAddrRequest   -> fwd
         -- SAddrReject    -> fwd
         SAddrPropose a m -> tryPutMVar tap_mv (a,m) >> return ()
         -- SAddrSelect _  -> fwd
         SUnknown       -> putStrLn "Error: Received an unknown message tag."
    where fwd = writeChan cmchan srcAddr msg
          sckWrtWrite = writeChan swchan
          writeChan c d m = atomically $ writeTChan c (d,m)
