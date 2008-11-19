module Scurry.State (
    ScurryState(..),
    StateRef,
    getState,
    alterState,
    getPeers,
    addPeer,
    delPeer,
    getEndPoint,
    getMAC,
    mkState,
) where

import Data.IORef
import Data.List
import Scurry.Types.Network

newtype StateRef = StateRef (IORef ScurryState)

-- | The state of the scurry application
data ScurryState = ScurryState {
    scurryPeers :: [(Maybe MACAddr,EndPoint)],
    scurryEndPoint :: EndPoint,
    scurryMAC :: MACAddr
} deriving (Show)

mkState :: ScurryState -> IO StateRef
mkState ss = (newIORef ss) >>= (return . StateRef)

getState :: StateRef -> IO ScurryState
getState (StateRef sr) = readIORef sr

alterState :: StateRef -> (ScurryState -> ScurryState) -> IO ()
alterState (StateRef sr) f = atomicModifyIORef sr (\s -> (f s, ()))

getPeers :: StateRef -> IO [(Maybe MACAddr,EndPoint)]
getPeers (StateRef sr) = (readIORef sr) >>= (return . scurryPeers)

addPeer :: StateRef -> (Maybe MACAddr, EndPoint) -> IO ()
addPeer sr (mac,ep) =
    let nubber (_,a) (_,b) = a == b
        ap ps = ps { scurryPeers = nubBy nubber ((mac,ep) : (scurryPeers ps)) }
    in alterState sr ap

delPeer :: StateRef -> EndPoint -> IO ()
delPeer sr ep = 
    let f s = filter (\(_,o) -> ep /= o) s 
        dp s = s { scurryPeers = f (scurryPeers s) }
    in alterState sr dp

getEndPoint :: StateRef -> IO EndPoint
getEndPoint (StateRef sr) = readIORef sr >>= (return . scurryEndPoint)

getMAC :: StateRef -> IO MACAddr
getMAC (StateRef sr) = readIORef sr >>= (return . scurryMAC)

