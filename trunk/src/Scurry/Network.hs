module Scurry.Network (
    ScurryNetwork(..),
) where

import Scurry.Types.Network

data ScurryNetwork = ScurryNetwork {
    scurryMask :: Maybe ScurryMask
} deriving (Show)
