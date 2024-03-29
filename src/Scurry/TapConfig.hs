{-# OPTIONS -XForeignFunctionInterface #-}

module Scurry.TapConfig(
    getTapHandle,
    closeTapHandle,
    read_tap,
    write_tap,
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU

import Scurry.Types.Network
import Scurry.Types.TAP

getTapHandle :: ScurryAddress -> ScurryMask -> IO (Either CInt (TapDesc,MACAddr))
getTapHandle ip mask = open_tap ip mask

closeTapHandle :: TapDesc -> IO ()
closeTapHandle = close_tap_ffi . unsafeForeignPtrToPtr

open_tap :: ScurryAddress -> ScurryAddress -> IO (Either CInt (TapDesc,MACAddr))
open_tap (ScurryAddress addr) (ScurryAddress mask) = do
    td' <- mkTapDesc

    let ti = (TapInfo td' $ MACAddr (255,255,255,255,255,255))

    ti' <- new ti -- MALLOC \
    res <- open_tap_ffi (fromIntegral addr) (fromIntegral mask) ti'
    (TapInfo _ mac) <- peek ti'
    free ti'      -- FREE   /

    if res < 0
        then return (Left res)
        else return $ Right (td',mac)

read_tap :: TapDesc -> IO BSI.ByteString
read_tap td = do
    let len = 1560
        ptd = unsafeForeignPtrToPtr td
    bs <- BSI.create len (\_ -> return ())
    r_len <- BSU.unsafeUseAsCString bs (\x -> read_tap_ffi ptd x (fromIntegral len))
    return (BSU.unsafeTake (fromIntegral r_len) bs)

write_tap :: TapDesc -> BSI.ByteString -> IO ()
write_tap td bs = do
    let ptd = unsafeForeignPtrToPtr td
    _ <- BSU.unsafeUseAsCString bs (\x -> write_tap_ffi ptd x ((fromIntegral . BSS.length) bs))
    return ()

foreign import ccall "help.h open_tap" open_tap_ffi :: CUInt -> CUInt -> (Ptr TapInfo) -> IO CInt
foreign import ccall "help.h close_tap" close_tap_ffi :: (Ptr TapDescX) -> IO ()
foreign import ccall "help.h read_tap" read_tap_ffi :: (Ptr TapDescX) -> CString -> CInt -> IO CInt
foreign import ccall "help.h write_tap" write_tap_ffi :: (Ptr TapDescX) -> CString -> CInt -> IO CInt
