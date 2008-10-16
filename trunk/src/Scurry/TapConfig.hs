{-# OPTIONS -ffi #-}

module Scurry.TapConfig(
    getTapHandle,
    closeTapHandle
) where

import Foreign.C.Types
import System.Posix.Types
import System.IO
import System.Posix.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)

getTapHandle :: String -> String -> IO (Either CInt Handle)
getTapHandle ip_str mask_str = do
    ip <- inet_addr ip_str
    mask <- inet_addr mask_str
    open_tap ip mask

closeTapHandle :: Handle -> IO ()
closeTapHandle tap = (handleToFd tap) >>= (close_tap_ffi . toEnum . fromEnum)

open_tap :: HostAddress -> HostAddress -> IO (Either CInt Handle)
open_tap addr mask = do
    tap <- open_tap_ffi (toEnum . fromEnum $ addr) (toEnum . fromEnum $ mask)
    if (tap < 0)
        then return (Left tap)
        else do h <- fdToHandle (Fd tap)
                return $ Right h

foreign import ccall "help.h open_tap" open_tap_ffi :: CUInt -> CUInt -> IO CInt
foreign import ccall "help.h close_tap" close_tap_ffi :: CInt -> IO ()
