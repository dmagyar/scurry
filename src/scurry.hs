module Main where

import System.Environment
import System.IO

import Network.Socket hiding (send, sendTo, recv, recvFrom, inet_addr, inet_ntoa)

import Scurry.Util
import Scurry.TapConfig
import Scurry.Comm
import Scurry.Management.Config
import Scurry.Management.Tracker
import Scurry.Types

main :: IO ()
main = do 
    (configPath:trackerPath:_) <- getArgs

    (Just config)  <- load_scurry_config_file configPath
    (Just tracker) <- load_tracker_file trackerPath

    let (Scurry (VpnConfig tapIp tapMask) (NetworkConfig mySockAddr)) = config
        yourSockAddrs = filter (\a -> a /= mySockAddr) $
            map tToS tracker

    putStrLn $ show yourSockAddrs

    let frmJst (Just x) = x
        frmJst Nothing = error "If you can't type an IP address right, I'm not even going to try and run."

    tap <- getTapHandle (frmJst (inet_ntoa tapIp)) (frmJst (inet_ntoa tapMask))

    case tap of
        (Left t)  -> putStrLn $ "Failed: " ++ (show t)
        (Right (t,mac)) -> doWork t mySockAddr (ScurryState yourSockAddrs mac)

    where
        tToS (ScurryPeer ip port) = SockAddrInet port ip 

doWork :: Handle -> SockAddr -> ScurryState -> IO ()
doWork tap mySockAddr state = do
    local <- prepEndPoint mySockAddr
    startCom tap local state
