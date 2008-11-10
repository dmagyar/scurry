{-# OPTIONS -ffi #-}

module Scurry.TapConfig(
    getTapHandle,
    closeTapHandle
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO
import GHC.Handle
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Scurry.Types

getTapHandle :: String -> String -> IO (Either CInt (Handle,MACAddr))
getTapHandle ip_str mask_str = do
    ip <- inet_addr ip_str
    mask <- inet_addr mask_str
    open_tap ip mask

closeTapHandle :: Handle -> IO ()
closeTapHandle tap = hClose tap -- (handleToFd tap) >>= (close_tap_ffi . toEnum . fromEnum)

open_tap :: HostAddress -> HostAddress -> IO (Either CInt (Handle,MACAddr))
open_tap addr mask = do
    ti' <- malloc -- MALLOC \
    res <- open_tap_ffi (toEnum . fromEnum $ addr) (toEnum . fromEnum $ mask) ti'
    (TapInfo tap mac) <- peek ti'
    free ti'      -- FREE   /

    if (res < 0)
        then return (Left res)
        else do h <- fdToHandle tap -- (Fd tap)
                return $ Right (h,mac)

foreign import ccall "help.h open_tap" open_tap_ffi :: CUInt -> CUInt -> (Ptr TapInfo) -> IO CInt
foreign import ccall "help.h close_tap" close_tap_ffi :: CInt -> IO ()
foreign import ccall "help.h read_tap" read_tap_ffi :: CInt -> CString -> CInt -> IO CInt
foreign import ccall "help.h write_tap" write_tap_ffi :: CInt -> CString -> CInt -> IO CInt
