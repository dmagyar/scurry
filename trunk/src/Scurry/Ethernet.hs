module Scurry.Ethernet where

import Data.Binary
import Data.Word
import qualified Data.ByteString.Lazy as BS
import Numeric

{- Why oh why does Control.Monad stop at liftM5 -}
liftM6 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do
    { x1 <- m1 ; x2 <- m2 ; x3 <- m3
    ; x4 <- m4 ; x5 <- m5 ; x6 <- m6
    ; return (f x1 x2 x3 x4 x5 x6) }

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

type EthernetChecksum = Word32
type EthernetPayload = BS.ByteString

data EthernetFrame = EthernetFrame EthernetHeader
                                   EthernetPayload
                                   EthernetChecksum

instance Show EthernetFrame where
    show (EthernetFrame h p c) = concat ["{EthFrm ", (show h), " :: ",
                                         (show $ BS.length p), " = 0x",
                                         (showHex c $ "}")]

instance Binary EthernetFrame where
    get = do
        { h <- get ; p <- get ; c <- get 
        ; return $ EthernetFrame h p c }
    put (EthernetFrame h p c) = do
        { put h ; put p ; put c }
