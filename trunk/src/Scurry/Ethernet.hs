module Scurry.Ethernet (
    SrcMAC,
    DstMAC,
    EthType,
    MACAddr(..),
    EthernetHeader(..),
) where

import Data.Binary
import Data.Word
import Numeric

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

