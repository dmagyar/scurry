module Scurry.Console (
    consoleThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.IORef
-- import GHC.Conc
import System.IO
import System.Exit

import Scurry.Comm.Message
import Scurry.Comm.Util
import Scurry.State

-- Console command interpreter

consoleThread :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
consoleThread ssRef chan = forever $ do
    ln <- getLine

    case ln of
         "peers" -> (readIORef ssRef) >>= (putStrLn . show)
         "shutdown" -> exitWith ExitSuccess
         u -> putStrLn $ "Unknown command: " ++ u
