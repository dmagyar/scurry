module Scurry.Comm(
    prepEndPoint,
    startCom,
    module Scurry.Comm.Message,
) where

import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import GHC.Conc
import Network.Socket (Socket(..), Family(..), socket, SocketType(..), defaultProtocol, bindSocket, setSocketOption, SocketOption(Broadcast))
import System.IO

import Scurry.GUI

import Scurry.Comm.Message
import Scurry.Comm.TapSource
import Scurry.Comm.TapWriter
import Scurry.Comm.SockSource
import Scurry.Comm.SockWrite
import Scurry.KeepAlive
import Scurry.Console
import Scurry.Comm.ConnectionManager

import Scurry.Util
import Scurry.State
import Scurry.Types.Network
import Scurry.TapConfig

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

startCom :: (Maybe ScurryAddress, Maybe ScurryMask) -> Socket -> ScurryState -> [EndPoint] -> IO ()
startCom tapCfg sock initSS eps = do
    sr <- mkState initSS -- Initial ScurryState
    swchan <- atomically newTChan -- SockWriter Channel
    cmchan <- atomically newTChan -- Connection Manager Channel
    twchan <- atomically newTChan -- TapWriter Channel

    tap_mv <- newEmptyMVar

    case tapCfg of
         (Just a, Just m) -> putMVar tap_mv (a,m)
         _                -> putStrLn "Requesting network settings from peers..."

    swt <- forkIO $ sockWriteThread sock swchan
    sst <- forkIO $ sockSourceThread twchan sock sr swchan cmchan tap_mv
    kat <- forkIO $ threadDelay (sToMs 5) >> keepAliveThread sr swchan tap_mv
    cmt <- forkIO $ conMgrThread sr swchan cmchan eps

    labelThread swt "Socket Write Thread"
    labelThread sst "Socket Source Thread"
    labelThread kat "Keep Alive Thread"
    labelThread cmt "Connection Manager Thread"

    helper <- forkIO $ do
        -- Use readMVar to prevent emptying the box.
        (tapaddr,tapmask) <- readMVar tap_mv

        putStrLn $ "Using TAP IP of " ++ (show tapaddr) ++ " and TAP netmask of " ++ (show tapmask)

        -- Bring up tap device
        Right (tap,macaddr) <- getTapHandle tapaddr tapmask 
        -- alterState sr (setMac (Just macaddr))

        updateMyMAC sr macaddr
        updateMyVPNAddr sr tapaddr
        updateNetMask sr tapmask

        twt <- forkIO $ tapWriterThread twchan tap
        tst <- forkIO $ tapSourceThread tap sr swchan
        labelThread tst "TAP Source Thread"
        labelThread twt "Tap Writer Thread"

    -- Helper thread to get me to the console while we wait for the TAP to come up.
    labelThread helper "Helper Thread"

    gui_t <- forkOS $ startGui

    labelThread gui_t "Gtk+ GUI Thread"
    
    -- Last thread is a continuation of the main thread
    consoleThread sr swchan

