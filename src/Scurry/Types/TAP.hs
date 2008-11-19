{-# OPTIONS -XEmptyDataDecls #-}
module Scurry.Types.TAP (
    TapDesc,
    TapDescX,
    TapInfo(..),
    mkTapDesc,
) where

import Data.Binary
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Scurry.Types.Network

-- | A TAP device descriptor. Since the C representation isn't uniform across
-- the different platforms we're trying to support, we're going to pull some
-- trickery with C unions. What the TapDesc type is going to do is hold a
-- pointer to the memory defining this struct. The TapDesc is passed to the
-- read/write/close calls to operate on the TAP device.
data TapDescX
type TapDesc = ForeignPtr TapDescX

instance Storable TapDescX where
    sizeOf    _ = 32
    alignment _ = 4
    peek _ = error "NONONO! NO CAN HAZ PEEK!!"
    poke _ = error "NONONO! NO CAN HAZ POKE!!"

mkTapDesc :: IO TapDesc
mkTapDesc = mallocForeignPtr

-- | TapInfo type. Holds information about a TAP device which is retreived
-- from the C bits of the applicatoin.
data TapInfo = TapInfo TapDesc MACAddr
    deriving (Show)

ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

-- | Storable instance for TapInfo
instance Storable TapInfo where
    sizeOf _    = 8 + ptrSize
    alignment _ = 4
    peek a = do
        w1 <- peekByteOff a (ptrSize + 0)
        w2 <- peekByteOff a (ptrSize + 1)
        w3 <- peekByteOff a (ptrSize + 2)
        w4 <- peekByteOff a (ptrSize + 3)
        w5 <- peekByteOff a (ptrSize + 4)
        w6 <- peekByteOff a (ptrSize + 5)
        return $ TapInfo undefined (MACAddr (w1,w2,w3,w4,w5,w6))

    poke a (TapInfo td (MACAddr (w1,w2,w3,w4,w5,w6))) = do
        pokeByteOff a 0 (unsafeForeignPtrToPtr td)
        pokeByteOff a (ptrSize + 0) w1
        pokeByteOff a (ptrSize + 1) w2
        pokeByteOff a (ptrSize + 2) w3
        pokeByteOff a (ptrSize + 3) w4
        pokeByteOff a (ptrSize + 4) w5
        pokeByteOff a (ptrSize + 5) w6
        pokeByteOff a (ptrSize + 6) (0 :: Word8)
        pokeByteOff a (ptrSize + 7) (0 :: Word8)

