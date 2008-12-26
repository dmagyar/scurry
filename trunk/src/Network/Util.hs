{-# OPTIONS -XForeignFunctionInterface #-}

module Network.Util (
    htonl,
    htons,
    ntohl,
    ntohs,
) where

import Data.Word

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32 
foreign import ccall unsafe "htons" htons :: Word16 -> Word16 
foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16

