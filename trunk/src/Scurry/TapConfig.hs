{-# OPTIONS -ffi #-}

module Scurry.TapConfig(
    getTapHandle,
    closeTapHandle
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Posix.Types
import System.IO
-- import System.Posix.IO #Job - removed, doesn't work on windows
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.Word

type MAC = (Word8,Word8,Word8,Word8,Word8,Word8)
data TapInfo = TapInfo CInt MAC
    deriving (Show)

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
        return $ TapInfo fd (w1,w2,w3,w4,w5,w6)

    poke a (TapInfo fd (w1,w2,w3,w4,w5,w6)) = do
        pokeByteOff a 0 fd
        pokeByteOff a 4 w1
        pokeByteOff a 5 w2
        pokeByteOff a 6 w3
        pokeByteOff a 7 w4
        pokeByteOff a 8 w5
        pokeByteOff a 9 w6
        pokeByteOff a 10 (0 :: Word8)
        pokeByteOff a 11 (0 :: Word8)


getTapHandle :: String -> String -> IO (Either CInt (Handle,MAC))
getTapHandle ip_str mask_str = do
    ip <- inet_addr ip_str
    mask <- inet_addr mask_str
    open_tap ip mask

closeTapHandle :: Handle -> IO ()
closeTapHandle tap = (handleToFd tap) >>= (close_tap_ffi . toEnum . fromEnum)

open_tap :: HostAddress -> HostAddress -> IO (Either CInt (Handle,MAC))
open_tap addr mask = do
    ti' <- malloc -- MALLOC \
    res <- open_tap_ffi (toEnum . fromEnum $ addr) (toEnum . fromEnum $ mask) ti'
    (TapInfo tap mac) <- peek ti'
    free ti'      -- FREE   /

    if (res < 0)
        then return (Left res)
        else do h <- fdToHandle (Fd tap)
                return $ Right (h,mac)

foreign import ccall "help.h open_tap" open_tap_ffi :: CUInt -> CUInt -> (Ptr TapInfo) -> IO CInt
foreign import ccall "help.h close_tap" close_tap_ffi :: CInt -> IO ()
