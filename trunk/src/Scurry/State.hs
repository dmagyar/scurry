module Scurry.State (
ScurryState(..)
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.Word

type MAC = (Word8,Word8,Word8,Word8,Word8,Word8)

data ScurryState = ScurryState [SockAddr] MAC
    deriving (Show)
