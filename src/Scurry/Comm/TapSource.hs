module Scurry.Comm.TapSource(
tapSourceThread 
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import GHC.Conc

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State

tapSourceThread :: Handle -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
tapSourceThread tap ssRef chan = forever $
    tapReader tap >>=
    (\x -> frameSwitch ssRef chan (tapDecode x))

tapReader :: Handle -> IO BS.ByteString
tapReader tap = do
    hWaitForInput tap (-1)
    BS.hGetNonBlocking tap readLength

frameSwitch :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> ScurryMsg -> IO ()
frameSwitch ssRef chan m = do
    (ScurryState peers) <- readIORef ssRef
    mapM_ (\x -> atomically $ writeTChan chan (DestSingle x,m)) peers

tapDecode :: BS.ByteString -> ScurryMsg
tapDecode bs = SFrame $ bsToEthernetTuple bs

