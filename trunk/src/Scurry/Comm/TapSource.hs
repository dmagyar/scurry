module Scurry.Comm.TapSource(
tapSourceThread 
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import System.IO
import qualified Data.ByteString as BSS
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.TapConfig
import Scurry.State
import Scurry.Types.TAP
import Scurry.Types.Threads

tapSourceThread :: TapDesc -> StateRef -> SockWriterChan -> IO ()
tapSourceThread tap sr chan = forever $
    read_tap tap >>=
    (\x -> frameSwitch sr chan (tapDecode x))

frameSwitch :: StateRef -> SockWriterChan -> ScurryMsg -> IO ()
frameSwitch sr chan m = do
    peers <- getPeers sr
    mapM_ (\x -> atomically $ writeTChan chan (DestSingle x,m)) (map (\(_,p) -> p) peers)

tapDecode :: BSS.ByteString -> ScurryMsg
tapDecode bs = SFrame $ bsToEthernetTuple bs

