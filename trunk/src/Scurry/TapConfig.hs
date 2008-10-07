{-# OPTIONS -ffi #-}

module Scurry.TapConfig where

import Foreign.C.Types
import System.Posix.Types
import System.IO
import System.Posix.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)

getTapHandle :: String -> IO (Either CInt Handle)
getTapHandle ip_str = (inet_addr ip_str) >>= open_tap

open_tap :: HostAddress -> IO (Either CInt Handle)
open_tap addr = do
    tap <- open_tap_ffi (toEnum . fromEnum $ addr)
    if (tap < 0)
        then return (Left tap)
        else do h <- fdToHandle (Fd tap)
                -- hSetBuffering h NoBuffering
                return $ Right h

foreign import ccall "help.h open_tap" open_tap_ffi :: CUInt -> IO CInt
foreign import ccall "help.h close_tap" close_tap_ffi :: CInt -> IO ()
