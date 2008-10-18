module Scurry.Comm(
    prepEndPoint,
    debugFrame,
    startCom,
    module Scurry.Comm.Message,
    module Scurry.State,
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
import Scurry.State

-- |Bind the socket to the specified socket address.
-- This specifies the network configuration we are using
-- as well.
prepEndPoint :: SockAddr -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s ep
    return s

startCom :: Handle -> Socket -> ScurryState -> IO ()
startCom tap sock initSS = do
    ssRef <- newIORef initSS
    chan  <- atomically $ newTChan

    forkIO $ tapSourceThread tap ssRef chan
    forkIO $ sockWriteThread sock chan

    -- Last thread is a continuation of the main thread
    sockSourceThread tap sock ssRef

