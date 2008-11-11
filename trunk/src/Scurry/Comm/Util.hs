module Scurry.Comm.Util (
    readLength,
    DestAddr(..),
    debugFrame,
    bsToEthernetTuple,
) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Network.Socket (SockAddr(..))

import Scurry.Types

readLength :: Int
readLength = 1560

data DestAddr = DestSingle SockAddr
              | DestList [SockAddr]
    deriving (Show)

bsToEthernetTuple :: BSS.ByteString -> (EthernetHeader,BSS.ByteString)
bsToEthernetTuple d = (decode (BS.fromChunks [d]), d)

-- |Takes an ethernet frame pair and prints some debug
-- information about it.
debugFrame :: (EthernetHeader,BSS.ByteString) -> IO ()
debugFrame (h,f) = do
    putStrLn $ concat [(show h)," => Length: ",(show $ BSS.length f)]

