module Main where

import System.Environment
import System.IO
import Control.Concurrent

import Network.Socket hiding (send, sendTo, recv, recvFrom, inet_addr, inet_ntoa)

import Scurry.Util
import Scurry.TapConfig
import Scurry.Communication
import Scurry.Management.Config
import Scurry.Management.Tracker

main :: IO ()
main = do 
    (configPath:trackerPath:_) <- getArgs

    (Just config)  <- load_scurry_config_file configPath
    (Just tracker) <- load_tracker_file trackerPath

    let (Scurry (VpnConfig tapIp _) (NetworkConfig mySock)) = config
        [ScurryPeer yourIp yourPort] = tracker

    tap <- getTapHandle $ (\(Just x) -> x) (inet_ntoa tapIp)

    let myAddr = mySock
        yourAddr = SockAddrInet yourPort yourIp

    case tap of
        (Left t)  -> putStrLn $ "Failed: " ++ (show t)
        (Right t) -> doWork t myAddr yourAddr

doWork :: Handle -> SockAddr -> SockAddr -> IO ()
doWork tap myIp yourIp = do
    local <- prepEndPoint myIp

    -- Send a thread off to read from the TAP device
    forkIO $ localProcessing tap local yourIp

    -- This thread will read from the network socket
    remoteProcessing tap local

