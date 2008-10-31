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

import Scurry.Console.Parser
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State

-- Console command interpreter

consoleThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
consoleThread ssRef chan = forever $ do
    ln <- getLine

    case (parseConsole ln) of
         (Left err) -> badCmd err
         (Right ln') -> goodCmd ln'

    where goodCmd cmd = case cmd of
                            Shutdown           -> exitWith ExitSuccess
                            ListPeers          -> (readIORef ssRef) >>= (putStrLn . show)
                            (NewPeer ha pn)    -> atomicModifyIORef ssRef (newPeer ha pn)
                            (RemovePeer ha pn) -> atomicModifyIORef ssRef (delPeer ha pn) 
          badCmd err = putStrLn $ "Bad Command: " ++ (show err)

newPeer :: HostAddress -> PortNumber -> ScurryState -> (ScurryState, ())
newPeer ha pn state = let o = addAddr state (SockAddrInet pn ha)
                      in (o,())
    where addAddr (ScurryState peers m ) sa = ScurryState (nub (sa : peers)) m

delPeer :: HostAddress -> PortNumber -> ScurryState -> (ScurryState, ())
delPeer ha pn state = (delAddr state (SockAddrInet pn ha),())
    where delAddr (ScurryState peers m) sa = ScurryState (filter (/= sa) peers) m
                    