module Scurry.Console (
    consoleThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.List
import System.IO
import System.Exit
import qualified GHC.Conc as GC

import Scurry.Console.Parser
import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.Types.Network
import Scurry.Types.Console
import Scurry.Types.Threads

import Scurry.State

-- Console command interpreter

consoleThread :: StateRef -> SockWriterChan -> IO ()
consoleThread sr chan = do
    (ScurryState peers _ mac) <- getState sr

    mapM_ (\(_,x) -> GC.atomically $ writeTChan chan (DestSingle x,SJoin mac)) peers

    forever $ do
        ln <- getLine

        case (parseConsole ln) of
             (Left err) -> badCmd err
             (Right ln') -> goodCmd ln'

    where goodCmd cmd = case cmd of
                             CmdShutdown           -> exitWith ExitSuccess
                             CmdListPeers          -> (getState sr) >>= (putStrLn . show)
                             (CmdNewPeer ha pn)    -> addPeer sr (Nothing, (EndPoint ha pn))
                             (CmdRemovePeer ha pn) -> delPeer sr (EndPoint ha pn)
          badCmd err = putStrLn $ "Bad Command: " ++ (show err)

