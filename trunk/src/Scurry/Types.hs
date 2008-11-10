module Scurry.Types(
    ScurryState(..),
    ConsoleCmd(..),
    TapDesc(..),
    TapInfo(..),
    SrcMAC,
    DstMAC,
    EthType,
    MACAddr(..),
    EthernetHeader(..),
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.Binary
import Numeric
import Foreign.C.Types
import Foreign.Storable

-- | A TAP device descriptor. Since the C representation isn't uniform across
-- the different platforms we're trying to support, we're going to pull some
-- trickery with C unions. What the TapDesc type is going to do is hold a
-- pointer to the memory defining this struct. The TapDesc is passed to the
-- read/write/close calls to operate on the TAP device.
newtype TapDesc = TapDesc ()

-- | MAC Address (6 8-bit words)
data MACAddr = MACAddr (Word8,Word8,Word8,Word8,Word8,Word8)

instance Show MACAddr where
    show (MACAddr (a,b,c,d,e,f)) = let s = flip showHex ":"
                                       l = flip showHex ""
                                   in concat [s a, s b, s c,
                                              s d, s e, l f]

instance Binary MACAddr where
    get = do
        { o1 <- get ; o2 <- get ; o3 <- get 
        ; o4 <- get ; o5 <- get ; o6 <- get 
        ; return $ MACAddr (o1,o2,o3,o4,o5,o6) }
    put (MACAddr (o1,o2,o3,o4,o5,o6)) = do
        { put o1 ; put o2 ; put o3
        ; put o4 ; put o5 ; put o6 }

-- | The state of the scurry application
data ScurryState = ScurryState [SockAddr] MACAddr
    deriving (Show)

-- | TapInfo type. Holds information about a TAP device which is retreived
-- from the C bits of the applicatoin.
data TapInfo = TapInfo CInt MACAddr
    deriving (Show)

-- | Storable instance for TapInfo
instance Storable TapInfo where
    sizeOf _    = 12
    alignment _ = 4
    peek a = do
        fd <- peekByteOff a 0
        w1 <- peekByteOff a 4
        w2 <- peekByteOff a 5
        w3 <- peekByteOff a 6
        w4 <- peekByteOff a 7
        w5 <- peekByteOff a 8
        w6 <- peekByteOff a 9
        return $ TapInfo fd (MACAddr (w1,w2,w3,w4,w5,w6))

    poke a (TapInfo fd (MACAddr (w1,w2,w3,w4,w5,w6)))= do
        pokeByteOff a 0 fd
        pokeByteOff a 4 w1
        pokeByteOff a 5 w2
        pokeByteOff a 6 w3
        pokeByteOff a 7 w4
        pokeByteOff a 8 w5
        pokeByteOff a 9 w6
        pokeByteOff a 10 (0 :: Word8)
        pokeByteOff a 11 (0 :: Word8)

-- | Datatype for Console commands
data ConsoleCmd = Shutdown
                | ListPeers
                | NewPeer HostAddress PortNumber
                | RemovePeer HostAddress PortNumber
    deriving (Show)

type SrcMAC = MACAddr
type DstMAC = MACAddr
type EthType = Word16

data EthernetHeader = EthernetHeader DstMAC SrcMAC EthType

instance Show EthernetHeader where
    show (EthernetHeader d s t) = concat ["{EthHdr ", (show s),
                                          " -> ", (show d),
                                          " :: 0x", (showHex t $ "}")]

instance Binary EthernetHeader where
    get = do
        { d <- get ; s <- get ; t <- get
        ; return $ EthernetHeader d s t }
    put (EthernetHeader d s t) = do
        { put d ; put s ; put t }

