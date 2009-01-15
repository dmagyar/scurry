module Main where

import System.Environment
import System.IO

import Network.Socket (withSocketsDo)

import Scurry.Peer
import Scurry.Comm
import Scurry.Management.Config
import Scurry.Management.Tracker
import Scurry.State
import Scurry.Network
import Scurry.Types.Network

main :: IO ()
main = withSocketsDo $ do 
    (configPath:trackerPath:_) <- getArgs

    (Just config)  <- load_scurry_config_file configPath
    (Just tracker) <- load_tracker_file trackerPath

    let (Scurry (VpnConfig tapIp tapMask) (NetworkConfig mySockAddr)) = config
        trackerEndPoints = filter (/= mySockAddr) $ map tToS tracker

    let initState = ScurryState {
            scurryPeers = [],
            scurryEndPoint = mySockAddr,
            scurryNetwork = ScurryNetwork {
                scurryMask = tapMask
            },
            scurryMyRecord = PeerRecord {
                peerMAC = Nothing,
                peerEndPoint = EndPoint (ScurryAddress 0) (ScurryPort 0),
                peerVPNAddr = tapIp,
                peerLocalPort = (\(EndPoint _ p) -> p) mySockAddr
            }
        }
        tapNet = (tapIp, tapMask)

    local <- prepEndPoint mySockAddr

    startCom tapNet local initState trackerEndPoints

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
