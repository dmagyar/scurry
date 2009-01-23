module Scurry.State (
    ScurryState(..),
    StateRef,
    getState,
    alterState,
    addPeer,
    delPeer,

    getPeers,
    getEndPoint,
    getVpnMask,
    getMAC,
    getLocalPort,
    getVPNAddr,
    getMyRecord,

    mkState,
) where

import Data.IORef
import Data.List
import Control.Monad
import Scurry.Types.Network
import Scurry.Network
import Scurry.Peer

newtype StateRef = StateRef (IORef ScurryState)

-- | The state of the scurry application
data ScurryState = ScurryState {
    scurryPeers :: [PeerRecord],
    scurryEndPoint :: EndPoint,
    scurryNetwork :: ScurryNetwork,
    scurryMyRecord :: PeerRecord
} deriving (Show)

mkState :: ScurryState -> IO StateRef
mkState = liftM StateRef . newIORef

getState :: StateRef -> IO ScurryState
getState (StateRef sr) = readIORef sr

alterState :: StateRef -> (ScurryState -> ScurryState) -> IO ()
alterState (StateRef sr) f = atomicModifyIORef sr (\s -> (f s, ()))

addPeer :: StateRef -> PeerRecord -> IO ()
addPeer sr pr =
    let nubber (PeerRecord { peerEndPoint = a })
               (PeerRecord { peerEndPoint = b }) = a == b
        np ps = ps { scurryPeers = nubBy nubber (pr : scurryPeers ps) }
    in alterState sr np

delPeer :: StateRef -> EndPoint -> IO ()
delPeer sr ep = 
    let f = filter (\(PeerRecord { peerEndPoint = o }) -> ep /= o)
        dp s = s { scurryPeers = f (scurryPeers s) }
    in alterState sr dp

getPeers :: StateRef -> IO [PeerRecord]
getPeers = extract scurryPeers

getEndPoint :: StateRef -> IO EndPoint
getEndPoint = extract scurryEndPoint

getVpnMask :: StateRef -> IO (Maybe ScurryMask)
getVpnMask = extract (scurryMask . scurryNetwork)

getMAC :: StateRef -> IO (Maybe MACAddr)
getMAC = extract (peerMAC . scurryMyRecord)

getLocalPort :: StateRef -> IO ScurryPort
getLocalPort = extract (peerLocalPort . scurryMyRecord)

getVPNAddr :: StateRef -> IO (Maybe ScurryAddress)
getVPNAddr = extract (peerVPNAddr . scurryMyRecord)

getMyRecord :: StateRef -> IO PeerRecord
getMyRecord = extract scurryMyRecord

extract :: (ScurryState -> a) -> StateRef -> IO a
extract e (StateRef sr) = liftM e (readIORef sr)
