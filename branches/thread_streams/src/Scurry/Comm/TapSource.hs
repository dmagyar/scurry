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
import Scurry.Types.Network

tapSourceThread :: TapDesc -> StateRef -> SockWriterChan -> IO ()
tapSourceThread tap sr chan = forever $
    read_tap tap >>=
    (\x -> frameSwitch sr chan (tapDecode x))

frameSwitch :: StateRef -> SockWriterChan -> ScurryMsg -> IO ()
frameSwitch sr chan m = do
    peers <- getPeers sr
    case m of 
      SFrame ((EthernetHeader dst _ _), _) -> 
        case (lookup (Just dst) peers) of 
          Just p  -> sendMsg p
          Nothing -> mapM_ sendMsg (map (\(_,p) -> p) peers)
        where sendMsg dest = atomically $ writeTChan chan (DestSingle dest, m)
      _ -> putStrLn $ "Error: Unexpected frame type from TAP"
    
tapDecode :: BSS.ByteString -> ScurryMsg
tapDecode bs = SFrame $ bsToEthernetTuple bs

