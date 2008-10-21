module Scurry.State (
ScurryState(..)
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)

data ScurryState = ScurryState [SockAddr]
    deriving (Show)
