module Scurry.Console.Parser (
) where

import Network.Socket (HostAddress,PortNumber)
import Text.Parsec

data ConsoleCmd = Shutdown
                | ListPeers
                | NewPeer HostAddress PortNumber
                | RemovePeer
    deriving (Show)
