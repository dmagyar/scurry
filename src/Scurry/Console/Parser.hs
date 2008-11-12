module Scurry.Console.Parser (
    parseConsole
) where

import Network.Socket (HostAddress,PortNumber)
import Text.Parsec
import Text.Parsec.String
import Scurry.Util
import Scurry.Types

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
    spaces
    (ip,port) <- ip_port_pair
    return $ NewPeer ip port

cmdRemovePeer :: Parser ConsoleCmd
cmdRemovePeer = do
    string "remove"
    spaces
    (ip,port) <- ip_port_pair
    return $ RemovePeer ip port

{- Mostly helper parsers that don't exist in the Parsec libary -}    
ip_port_pair :: Parser (HostAddress,PortNumber)
ip_port_pair = do
    ip <- ip_str
    char ':'
    port <- many1 digit

    let ip'   = inet_addr ip
        port' = (read port :: Integer)

    if (port' > 65535 || port' < 0)
       then parserFail "Not a valid port."
       else case ip' of
                 (Just ip'') -> return (ip'',fromIntegral port')
                 Nothing     -> parserFail "Not an ip address."

ip_str :: Parser String
ip_str = do
    q1 <- quad
    char '.'
    q2 <- quad
    char '.'
    q3 <- quad
    char '.'
    q4 <- quad
    return $ let dot = "."
             in concat [q1,dot,q2,dot,q3,dot,q4]
    where
        quad = choice [try $ count 3 digit,
                       try $ count 2 digit,
                       try $ count 1 digit]