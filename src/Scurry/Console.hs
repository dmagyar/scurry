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
import qualified GHC.Conc as GC

import Scurry.Console.Parser
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.Types

-- Console command interpreter

consoleThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
consoleThread ssRef chan = do
    (ScurryState peers mac) <- readIORef ssRef

    mapM_ (\x -> GC.atomically $ writeTChan chan (DestSingle x,SJoin)) peers

    forever $ do
        ln <- getLine

        case (parseConsole ln) of
             (Left err) -> badCmd err
             (Right ln') -> goodCmd ln'

    where goodCmd cmd = case cmd of
                            CmdShutdown           -> exitWith ExitSuccess
                            CmdListPeers          -> (readIORef ssRef) >>= (putStrLn . show)
                            (CmdNewPeer ha pn)    -> atomicModifyIORef ssRef (newPeer ha pn)
                            (CmdRemovePeer ha pn) -> atomicModifyIORef ssRef (delPeer ha pn) 
          badCmd err = putStrLn $ "Bad Command: " ++ (show err)

newPeer :: HostAddress -> PortNumber -> ScurryState -> (ScurryState, ())
newPeer ha pn state = let o = addAddr state (SockAddrInet pn ha)
                      in (o,())
    where addAddr (ScurryState peers m ) sa = ScurryState (nub (sa : peers)) m

delPeer :: HostAddress -> PortNumber -> ScurryState -> (ScurryState, ())
delPeer ha pn state = (delAddr state (SockAddrInet pn ha),())
    where delAddr (ScurryState peers m) sa = ScurryState (filter (/= sa) peers) m
                    
