module Scurry.Comm(
    prepEndPoint,
    debugFrame,
    startCom,
    module Scurry.Comm.Message,
) where


import Control.Concurrent.STM.TChan
import GHC.Conc
import Network.Socket (Socket(..), Family(..), socket, SocketType(..), defaultProtocol, bindSocket, setSocketOption, SocketOption(Broadcast))
import System.IO

import Scurry.Comm.Message
import Scurry.Comm.TapSource
import Scurry.Comm.TapWriter
import Scurry.Comm.SockSource
import Scurry.Comm.SockWrite
import Scurry.Comm.Util
import Scurry.KeepAlive
import Scurry.Console
import Scurry.Comm.ConnectionManager

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

    -- Set the broadcast option for IP packets (4) on the socket
    setSocketOption s Broadcast 4
    return s

startCom :: TapDesc -> Socket -> ScurryState -> [EndPoint] -> IO ()
startCom tap sock initSS eps = do
    sr <- mkState initSS -- Initial ScurryState
    swchan <- atomically newTChan -- SockWriter Channel
    cmchan <- atomically newTChan -- Connection Manager Channel
    twchan <- atomically newTChan -- TapWriter Channel

    swt <- forkIO $ sockWriteThread sock swchan
    sst <- forkIO $ sockSourceThread twchan sock sr swchan cmchan
    kat <- forkIO $ keepAliveThread sr swchan

    -- TODO: Bring up tap device here

    cmt <- forkIO $ conMgrThread sr swchan cmchan eps
    twt <- forkIO $ tapWriterThread twchan tap
    tst <- forkIO $ tapSourceThread tap sr swchan

    -- For debugging
    labelThread tst "TAP Source Thread"
    labelThread swt "Socket Write Thread"
    labelThread sst "Socket Source Thread"
    labelThread kat "Keep Alive Thread"
    labelThread cmt "Connection Manager Thread"
    labelThread twt "Tap Writer Thread"

    -- Last thread is a continuation of the main thread
    consoleThread sr swchan

