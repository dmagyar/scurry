module Scurry.Peer (
    PeerRecord(..),
    VPNAddr,
    LocalPort,
) where

import Data.Binary
import Scurry.Types.Network

type VPNAddr = ScurryAddress
type LocalPort = ScurryPort

data PeerRecord = PeerRecord {
    peerMAC :: MACAddr,
    peerEndPoint :: EndPoint,
    peerVPNAddr :: VPNAddr,
    peerLocalPort :: LocalPort
} deriving (Show)

-- | Binary Instance for PeerRecord
instance Binary PeerRecord where
    get = do
        m <- get
        e <- get
        v <- get
        p <- get
        return $ PeerRecord {
            peerMAC = m,
            peerEndPoint = e,
            peerVPNAddr = v,
            peerLocalPort = p
        }
    put p = do
        put (peerMAC p)
        put (peerEndPoint p)
        put (peerVPNAddr p)
        put (peerLocalPort p)
