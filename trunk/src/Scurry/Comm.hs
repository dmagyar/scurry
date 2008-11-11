module Scurry.Comm(
    prepEndPoint,
    debugFrame,
    startCom,
    module Scurry.Comm.Message,
) where


import Control.Concurrent.STM.TChan
import Data.IORef
import GHC.Conc
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.IO

import Scurry.Comm.Message
import Scurry.Comm.TapSource
import Scurry.Comm.SockSource
import Scurry.Comm.SockWrite
import Scurry.Comm.Util
import Scurry.KeepAlive
import Scurry.Console
import Scurry.Types

-- |Bind the socket to the specified socket address.
-- This specifies the network configuration we are using
-- as well.
prepEndPoint :: SockAddr -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s ep
    return s

startCom :: TapDesc -> Socket -> ScurryState -> IO ()
startCom tap sock initSS = do
    ssRef <- newIORef initSS
    chan  <- atomically $ newTChan

    tst <- forkIO $ tapSourceThread tap ssRef chan
    swt <- forkIO $ sockWriteThread sock chan
    sst <- forkIO $ sockSourceThread tap sock ssRef
    kat <- forkIO $ keepAliveThread ssRef chan

    -- For debugging
    labelThread tst "TAP Source Thread"
    labelThread swt "Socket Write Thread"
    labelThread sst "Socket Source Thread"
    labelThread kat "Keep Alive Thread"

    -- Last thread is a continuation of the main thread
    consoleThread ssRef chan
