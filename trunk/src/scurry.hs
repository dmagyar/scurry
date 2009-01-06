module Main where

import System.Environment
import System.IO

import Network.Socket (Socket(..),withSocketsDo)

import Scurry.Util
import Scurry.Peer
import Scurry.TapConfig
import Scurry.Comm
import Scurry.Management.Config
import Scurry.Management.Tracker
import Scurry.State
import Scurry.Types.Network
import Scurry.Comm.SockSource (sockReadMsg)
import Scurry.Comm.SockWrite (sendToAddr)

import Control.Concurrent (threadDelay)
-- import Scurry.Types.TAP

main :: IO ()
main = withSocketsDo $ do 
    (configPath:trackerPath:_) <- getArgs

    (Just config)  <- load_scurry_config_file configPath
    (Just tracker) <- load_tracker_file trackerPath

    let (Scurry (VpnConfig tapIp tapMask) (NetworkConfig mySockAddr)) = config
        trackerEndPoints = filter (/= mySockAddr) $ map tToS tracker

    let frmJst (Just x) = x
        frmJst Nothing = error "If you can't type an IP address right, I'm not even going to try and run."
        mkMyState mac = ScurryState {
            scurryPeers = [],
            scurryEndPoint = mySockAddr,
            scurryVpnMask = tapMask,
            scurryMyRecord = PeerRecord {
                peerMAC = mac,
                peerEndPoint = EndPoint (ScurryAddress 0) (ScurryPort 0),
                peerVPNAddr = tapIp,
                peerLocalPort = (\(EndPoint _ p) -> p) mySockAddr
            }
        }

    local <- prepEndPoint mySockAddr

    -- request an address and network settings

    tap <- getTapHandle (frmJst (inet_ntoa tapIp)) (frmJst (inet_ntoa tapMask))

    case tap of
        (Left t)        -> putStrLn $ "Failed: " ++ show t
        (Right (t,mac)) -> startCom t local (mkMyState mac) trackerEndPoints

    where
        tToS (ScurryPeer ip port) = EndPoint ip port 


{-
getAddress :: Socket -> [EndPoint] -> IO (ScurryAddress, ScurryMask)
getAddress sock (t:trackers) = do
    let msg = SAddrRequest
        cmd = sendToAddr sock msg
    
    putStrLn "Requesting address..."
    cmd t

    where
        delayRead = do
            threadDelay (sToMs 1.5)
-}            
