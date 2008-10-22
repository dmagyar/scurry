module Scurry.Console (
    consoleThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.IORef
import Data.List
-- import GHC.Conc
import System.IO
import System.Exit
import Network.Socket hiding (inet_addr,inet_ntoa)
import Numeric

import Scurry.Console.Parser
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.Util
import Scurry.State

-- Console command interpreter

consoleThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
consoleThread ssRef chan = forever $ do
    let rssr = readIORef ssRef
    ln <- getLine

    case ln of
         "peers"    -> rssr >>= (putStrLn . show)
         "shutdown" -> exitWith ExitSuccess
         "newpeer"  -> putStrLn "Peer: " >> getLine >>= (\l -> atomicModifyIORef ssRef (newPeer l))
         u          -> putStrLn $ "Unknown command: " ++ u

newPeer :: String -> ScurryState -> (ScurryState, ())
newPeer addr state = let (ip,port) = break (== ':') addr
                         port' = drop 1 port
                         parsed_ip = inet_addr ip
                         parsed_port = readDec port'
                         o = case (parsed_ip,parsed_port) of
                                  (Just ip',[(port'',_)]) -> addAddr state (SockAddrInet port'' ip')
                                  _ -> state
                     in (o,())
    where addAddr (ScurryState peers) sa = ScurryState $ nub (sa : peers)
