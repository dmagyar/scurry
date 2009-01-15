module Scurry.Comm.Util (
    readLength,
    DestAddr(..),
    -- debugFrame,
    bsToEthHdr,
) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import Scurry.Types.Network

readLength :: Int
readLength = 1560


data DestAddr = DestSingle EndPoint
              | DestList [EndPoint]
    deriving (Show)

bsToEthHdr :: BSS.ByteString -> EthernetHeader
bsToEthHdr d = decode (BS.fromChunks [d])

{-
-- |Takes an ethernet frame pair and prints some debug
-- information about it.
debugFrame :: (EthernetHeader,BSS.ByteString) -> IO ()
debugFrame (h,f) = putStrLn $ concat [(show h)," => Length: ",(show $ BSS.length f)]
-}

