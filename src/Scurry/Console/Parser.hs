module Scurry.Console.Parser (
    ConsoleCmd(..),
    parseConsole
) where

import Network.Socket (HostAddress,PortNumber)
import Text.Parsec
import Text.Parsec.String

data ConsoleCmd = Shutdown
                | ListPeers
                | NewPeer HostAddress PortNumber
                | RemovePeer
    deriving (Show)

parseConsole :: String -> Either ParseError ConsoleCmd
parseConsole = parse consoleCmd "Console"

consoleCmd :: Parser ConsoleCmd
consoleCmd = (try cmdShutdown) <|>
             (try cmdListPeers) <|>
             (try cmdNewPeer) <|>
             (try cmdRemovePeer) <|>
             (fail "Command not recognized")

cmdShutdown :: Parser ConsoleCmd
cmdShutdown = do
    string "shutdown"
    return Shutdown

cmdListPeers :: Parser ConsoleCmd
cmdListPeers = do
    string "peers"
    return ListPeers

cmdNewPeer :: Parser ConsoleCmd
cmdNewPeer = do
    string "new"
    return $ NewPeer 0 0

cmdRemovePeer :: Parser ConsoleCmd
cmdRemovePeer = do
    string "remove"
    return RemovePeer
