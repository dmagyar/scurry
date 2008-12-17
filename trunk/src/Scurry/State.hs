module Scurry.State (
    ScurryState(..),
    StateRef,
    getState,
    alterState,
    addPeer,
    delPeer,

    getPeers,
    getEndPoint,
    getMAC,
    getLocalPort,
    getVPNAddr,
    getMyRecord,

    mkState,
) where

import Data.IORef
import Data.List
import Scurry.Types.Network
import Scurry.Peer

newtype StateRef = StateRef (IORef ScurryState)

-- | The state of the scurry application
data ScurryState = ScurryState {
    scurryPeers :: [PeerRecord],
    scurryEndPoint :: EndPoint,
    scurryMyRecord :: PeerRecord
} deriving (Show)

mkState :: ScurryState -> IO StateRef
mkState ss = (newIORef ss) >>= (return . StateRef)

getState :: StateRef -> IO ScurryState
getState (StateRef sr) = readIORef sr

alterState :: StateRef -> (ScurryState -> ScurryState) -> IO ()
alterState (StateRef sr) f = atomicModifyIORef sr (\s -> (f s, ()))

addPeer :: StateRef -> PeerRecord -> IO ()
addPeer sr pr =
    let nubber (PeerRecord { peerEndPoint = a })
               (PeerRecord { peerEndPoint = b }) = a == b
        ap ps = ps { scurryPeers = nubBy nubber (pr : (scurryPeers ps)) }
    in alterState sr ap

delPeer :: StateRef -> EndPoint -> IO ()
delPeer sr ep = 
    let f = filter (\(PeerRecord { peerEndPoint = o }) -> ep /= o)
        dp s = s { scurryPeers = f (scurryPeers s) }
    in alterState sr dp

getPeers :: StateRef -> IO [PeerRecord]
getPeers = extract scurryPeers

getEndPoint :: StateRef -> IO EndPoint
getEndPoint = extract scurryEndPoint

getMAC :: StateRef -> IO MACAddr
getMAC = extract (peerMAC . scurryMyRecord)

getLocalPort :: StateRef -> IO ScurryPort
getLocalPort = extract (peerLocalPort . scurryMyRecord)

getVPNAddr :: StateRef -> IO ScurryAddress
getVPNAddr = extract (peerVPNAddr . scurryMyRecord)

getMyRecord :: StateRef -> IO PeerRecord
getMyRecord = extract scurryMyRecord

extract :: (ScurryState -> a) -> StateRef -> IO a
extract e (StateRef sr) = readIORef sr >>= (return . e)
