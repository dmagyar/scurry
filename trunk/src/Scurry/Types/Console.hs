module Scurry.Types.Console (
    ConsoleCmd(..),
) where

import Scurry.Types.Network

-- | Datatype for Console commands
data ConsoleCmd = CmdShutdown
                | CmdListPeers
                | CmdNewPeer ScurryAddress ScurryPort
                | CmdRemovePeer ScurryAddress ScurryPort
    deriving (Show)

