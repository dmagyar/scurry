module Scurry.Comm.SockSource (
sockSourceThread,
sockReadMsg,
) where

import Control.Monad (forever)
import Data.Binary
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import System.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.STM.TChan
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State
import Scurry.Types.Network
import Scurry.Types.Threads

sockSourceThread :: TapWriterChan -> Socket -> StateRef -> SockWriterChan -> ConMgrChan -> IO ()
sockSourceThread tap sock sr swchan cmchan = forever $ do
    (addr,msg) <- sockReader sock
    routeInfo tap sr swchan cmchan (addr,sockDecode msg)
    return ()
    
sockReader :: Socket -> IO (EndPoint,BSS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (saToEp addr,msg)

routeInfo :: TapWriterChan -> StateRef -> SockWriterChan -> ConMgrChan -> (EndPoint,ScurryMsg) -> IO ()
routeInfo tap _ swchan cmchan (srcAddr,msg) =
    case msg of
         SFrame (_,frame) -> atomically $ writeTChan tap frame
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
         SAddrPropose _ _ -> fwd
         -- SAddrSelect _  -> fwd
         SUnknown       -> putStrLn "Error: Received an unknown message tag."
    where fwd = writeChan cmchan srcAddr msg
          sckWrtWrite = writeChan swchan
          writeChan c d m = atomically $ writeTChan c (d,m)

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])


sockReadMsg :: Socket -> IO (EndPoint, ScurryMsg)
sockReadMsg sock = do
    (buffer,addr) <- recvFrom sock readLength
    return (saToEp addr, sockDecode buffer)
