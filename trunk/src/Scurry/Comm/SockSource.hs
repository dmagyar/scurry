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

import Scurry.TapConfig
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.Types

sockSourceThread :: TapDesc -> Socket -> (IORef ScurryState) -> IO ()
sockSourceThread tap sock ssRef = forever $
    (sockReader sock) >>=
    (\(addr,msg) -> routeInfo tap ssRef (addr,sockDecode msg))
    
sockReader :: Socket -> IO (SockAddr,BSS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (addr,msg)

routeInfo :: TapDesc -> (IORef ScurryState) -> (SockAddr,ScurryMsg) -> IO ()
routeInfo tap ssRef (srcAddr,msg) = do
    case msg of
         SFrame (_,frame) -> write_tap tap frame
         SJoin            -> atomicModifyIORef ssRef updatePeers
         SKeepAlive       -> error "SKeepAlive not supported"
         SNotifyPeer _    -> error "SNotifyPeer not supported"
         SRequestPeer     -> error "SRequestPeer not supported"
         SUnknown         -> error "SUnknown not supported"
    where updatePeers ss@(ScurryState ps m) = if elem srcAddr ps
                                                 then (ss,())
                                                 else (ScurryState (srcAddr : ps) m,())

sockDecode :: BSS.ByteString -> ScurryMsg
sockDecode msg = decode (BS.fromChunks [msg])

{-
tapWriter :: Handle -> BS.ByteString -> IO ()
tapWriter tap frame = do
    BSS.hPut tap (BSS.concat . BS.toChunks $ frame)
    hFlush tap
-}
