module Main where

import System.Environment
import System.IO

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

    let (Scurry (VpnConfig tapIp _) (NetworkConfig mySockAddr)) = config
        yourSockAddrs = filter (\a -> a /= mySockAddr) $
            map tToS tracker

    putStrLn $ show yourSockAddrs

    tap <- getTapHandle $ (\(Just x) -> x) (inet_ntoa tapIp)

    case tap of
        (Left t)  -> putStrLn $ "Failed: " ++ (show t)
        (Right t) -> doWork t mySockAddr yourSockAddrs

    where
        tToS (ScurryPeer ip port) = SockAddrInet port ip 

doWork :: Handle -> SockAddr -> [SockAddr] -> IO ()
doWork tap mySockAddr yourSockAddrs = do
    local <- prepEndPoint mySockAddr

    -- Send a thread off to read from the TAP device
    -- forkIO $ localProcessing tap local yourSockAddrs

    -- This thread will read from the network socket
    -- remoteProcessing tap local
    
    startCom tap local (ScurryState yourSockAddrs)

