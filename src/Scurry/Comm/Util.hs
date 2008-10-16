module Scurry.Comm.Util (
readLength,
DestAddr(..),
debugFrame,
bsToEthernetTuple,
) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Network.Socket (SockAddr(..))

import Scurry.Ethernet

readLength :: Int
readLength = 1500

data DestAddr = DestSingle SockAddr
              | DestList [SockAddr]
    deriving (Show)

bsToEthernetTuple :: BS.ByteString -> (EthernetHeader,BS.ByteString)
bsToEthernetTuple d = (decode d, d)

-- |Takes an ethernet frame pair and prints some debug
-- information about it.
debugFrame :: (EthernetHeader,BS.ByteString) -> IO ()
debugFrame (h,f) = do
    putStrLn $ concat [(show h)," => Length: ",(show $ BS.length f)]

