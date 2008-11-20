module Scurry.Comm(
    prepEndPoint,
    debugFrame,
    startCom,
    module Scurry.Comm.Message,
) where


import Control.Concurrent.STM.TChan
import GHC.Conc
import Network.Socket (Socket(..), Family(..), socket, SocketType(..), defaultProtocol, bindSocket)
import System.IO

import Scurry.Comm.Message
import Scurry.Comm.TapSource
import Scurry.Comm.SockSource
import Scurry.Comm.SockWrite
import Scurry.Comm.Util
import Scurry.KeepAlive
import Scurry.Console
import Scurry.ConnectionManager

import Scurry.State
import Scurry.Types.Network
import Scurry.Types.TAP

-- |Bind the socket to the specified socket address.
-- This specifies the network configuration we are using
-- as well.
prepEndPoint :: EndPoint -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s (epToSa ep)
    return s

startCom :: TapDesc -> Socket -> ScurryState -> IO ()
startCom tap sock initSS = do
    sr <- mkState initSS -- Initial ScurryState
    swchan <- atomically $ newTChan -- SockWriter Channel
    cmchan <- atomically $ newTChan -- Connection Manager Channel

    tst <- forkIO $ tapSourceThread tap sr swchan
    swt <- forkIO $ sockWriteThread sock swchan
    sst <- forkIO $ sockSourceThread tap sock sr swchan cmchan
    kat <- forkIO $ keepAliveThread sr swchan
    cmt <- forkIO $ conMgrThread sr swchan cmchan

    -- For debugging
    labelThread tst "TAP Source Thread"
    labelThread swt "Socket Write Thread"
    labelThread sst "Socket Source Thread"
    labelThread kat "Keep Alive Thread"
    labelThread cmt "Connection Manager Thread"

    -- Last thread is a continuation of the main thread
    consoleThread sr swchan

