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
import Scurry.Peer

-- Console command interpreter

consoleThread :: StateRef -> SockWriterChan -> IO ()
consoleThread sr chan = do
    (ScurryState {scurryPeers = peers, scurryMyRecord = rec}) <- getState sr

    mapM_ (\(PeerRecord { peerEndPoint = ep }) -> GC.atomically $ writeTChan chan (DestSingle ep,SJoin rec)) peers

    forever $ do
        ln <- getLine

        case parseConsole ln of
             (Left err) -> badCmd err
             (Right ln') -> goodCmd ln'

    where goodCmd cmd = case cmd of
                             CmdShutdown           -> exitWith ExitSuccess
                             CmdListPeers          -> getState sr >>= print
                             {- TODO: Find a way to do this... -}
                             {- (CmdNewPeer ha pn)    -> addPeer sr (Nothing, (EndPoint ha pn)) -}
                             CmdNewPeer _ _        -> putStrLn "CmdNewPeer disabled for now..."
                             (CmdRemovePeer ha pn) -> delPeer sr (EndPoint ha pn)
          badCmd err = putStrLn $ "Bad Command: " ++ show err

