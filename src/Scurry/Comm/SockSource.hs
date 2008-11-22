module Scurry.Comm.SockSource (
sockSourceThread
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
routeInfo tap _ swchan cmchan (srcAddr,msg) = do

    case msg of
         SFrame (_,frame) -> write_tap tap frame
         SKeepAlive     -> conMgrFwd
         SJoin _        -> conMgrFwd
         SJoinReply _ _ -> conMgrFwd
         SNotifyPeer _  -> conMgrFwd
         SRequestPeer   -> putStrLn "Error: SRequestPeer not supported"
         SPing pid      -> sckWrtWrite (DestSingle srcAddr) (SEcho pid)
         SEcho eid      -> putStrLn $ "Echo: " ++ (show eid) ++ (show $ srcAddr)
         SUnknown       -> putStrLn $ "Error: Received an unknown message tag."
    where conMgrFwd = writeChan cmchan srcAddr msg
          sckWrtWrite d m = writeChan swchan d m
          writeChan c d m = atomically $ writeTChan c (d,m)

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])

