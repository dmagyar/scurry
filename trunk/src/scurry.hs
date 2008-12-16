module Main where

import System.Environment
import System.IO

import Network.Socket (withSocketsDo)

import Scurry.Util
import Scurry.TapConfig
import Scurry.Comm
import Scurry.Management.Config
import Scurry.Management.Tracker
import Scurry.State
import Scurry.Types.Network
import Scurry.Types.TAP

main :: IO ()
main = withSocketsDo $ do 
    (configPath:trackerPath:_) <- getArgs

    (Just config)  <- load_scurry_config_file configPath
    (Just tracker) <- load_tracker_file trackerPath

    let (Scurry (VpnConfig tapIp tapMask) (NetworkConfig mySockAddr)) = config
        trackerEndPoints = filter (/= mySockAddr) $ map tToS tracker

    let frmJst (Just x) = x
        frmJst Nothing = error "If you can't type an IP address right, I'm not even going to try and run."

    tap <- getTapHandle (frmJst (inet_ntoa tapIp)) (frmJst (inet_ntoa tapMask))

    case tap of
        (Left t)        -> putStrLn $ "Failed: " ++ (show t)
        (Right (t,mac)) -> doWork t mySockAddr (ScurryState [] mySockAddr mac) trackerEndPoints

    where
        tToS (ScurryPeer ip port) = EndPoint ip port 

doWork :: TapDesc -> EndPoint -> ScurryState -> [EndPoint] -> IO ()
doWork tap mySockAddr state teps = do
    local <- prepEndPoint mySockAddr
    startCom tap local state teps
