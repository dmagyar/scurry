{-# OPTIONS -XGeneralizedNewtypeDeriving #-}

module Scurry.Types.Network (
    MACAddr(..),
    EthernetHeader(..),
    SrcMAC,
    DstMAC,
    EthType,
    EndPoint(..),
    ScurryAddress(..),
    ScurryMask,
    ScurryPort(..),
    epToSa,
    saToEp,
) where

import Data.Binary
import Numeric
import Network.Socket (inet_ntoa,HostAddress,PortNumber(..),SockAddr(..))
import Network.Util
import Control.Monad (liftM,liftM2)
import System.IO.Unsafe (unsafePerformIO)

-- | MAC Address (6 8-bit words)
data MACAddr = MACAddr (Word8,Word8,Word8,Word8,Word8,Word8)
    deriving (Eq,Ord)

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

data EthernetHeader = EthernetHeader DstMAC SrcMAC EthType
type SrcMAC = MACAddr
type DstMAC = MACAddr
type EthType = Word16

instance Show EthernetHeader where
    show (EthernetHeader d s t) = concat ["{EthHdr ", (show s),
                                          " -> ", (show d),
                                          " :: 0x", (showHex t "}")]

instance Binary EthernetHeader where
    get = do
        { d <- get ; s <- get ; t <- get
        ; return $ EthernetHeader d s t }
    put (EthernetHeader d s t) = do
        { put d ; put s ; put t }

type ScurryMask = ScurryAddress

newtype ScurryAddress = ScurryAddress {
    scurryAddr :: HostAddress
} deriving (Eq,Ord)

instance Enum ScurryAddress where
    succ (ScurryAddress a) = ScurryAddress $ htonl $ ntohl a + 1
    pred (ScurryAddress a) = ScurryAddress $ htonl $ ntohl a - 1
    toEnum = ScurryAddress . htonl . fromIntegral
    fromEnum (ScurryAddress a) = fromEnum (ntohl a)

instance Show ScurryAddress where
    -- We use unsafePerformIO here to avoid referencing the
    -- version of inet_ntoa in Scurry.Util since this would
    -- cause a cycle. TODO: Fix this later.
    show (ScurryAddress ha) = unsafePerformIO $ inet_ntoa ha

newtype ScurryPort = ScurryPort PortNumber
    deriving (Eq,Ord)

instance Show ScurryPort where
    show (ScurryPort pn) = show pn

data EndPoint = EndPoint ScurryAddress ScurryPort
    deriving (Show,Eq,Ord)

epToSa :: EndPoint -> SockAddr
epToSa (EndPoint (ScurryAddress ha) (ScurryPort pn)) = SockAddrInet pn ha 

saToEp :: SockAddr -> EndPoint
saToEp (SockAddrInet pn ha) = EndPoint (ScurryAddress ha) (ScurryPort pn)
saToEp _ = error "ERROR: Only IPV4 addresses are supported for now."

instance Binary ScurryPort where
    get = liftM (ScurryPort . PortNum) get
    put (ScurryPort (PortNum p)) = put p

instance Binary ScurryAddress where
    get = liftM ScurryAddress get
    put (ScurryAddress ha) = put ha

instance Binary EndPoint where
    get = do tag <- getWord8
             case tag of
                  0 -> liftM2 EndPoint get get
                  -- 1 -> liftM4 SockAddrInet6 get get get get -- #Job removed, not compatable with windows
                  -- 2 -> liftM SockAddrUnix get
                  _ -> error "Not a EndPoint"
    put (EndPoint sa sp) =
        do putWord8 0
           put sa
           put sp
    -- #Job - removed, not compatable with windows
    -- put (SockAddrInet6 pn fi ha si) =
        -- do putWord8 1
           -- put pn
           -- put fi
           -- put ha
           -- put si
    -- put (SockAddrUnix s) =
        -- do putWord8 2
           -- put s

