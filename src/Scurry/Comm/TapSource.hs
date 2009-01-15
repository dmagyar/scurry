module Scurry.Comm.TapSource(
tapSourceThread 
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import System.IO
import qualified Data.ByteString as BSS
import GHC.Conc
import Data.List (find)
import Data.Maybe

import Scurry.Peer
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.TapConfig
import Scurry.State
import Scurry.Types.TAP
import Scurry.Types.Threads
import Scurry.Types.Network

tapSourceThread :: TapDesc -> StateRef -> SockWriterChan -> IO ()
tapSourceThread tap sr chan = forever $
    read_tap tap >>= ((frameSwitch sr chan) . tapDecode)

frameSwitch :: StateRef -> SockWriterChan -> ScurryMsg -> IO ()
frameSwitch sr chan m@(SFrame bs) = do
    peers <- getPeers sr

    let (EthernetHeader dst _ _) = bsToEthHdr bs
        sendMsg dest = atomically $ writeTChan chan (DestSingle dest, m)

    case find (\pr -> (Just dst) == peerMAC pr) peers of 
      Just p  -> sendMsg (peerEndPoint p)
      Nothing -> mapM_ sendMsg (map peerEndPoint peers)
frameSwitch _ _ _ = error "Unexpected ScurryMsg sent to frameSwitch."
    
tapDecode :: BSS.ByteString -> ScurryMsg
tapDecode = SFrame

