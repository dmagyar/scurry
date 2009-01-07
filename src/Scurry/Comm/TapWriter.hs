module Scurry.Comm.TapWriter (
    tapWriterThread,
) where

import Control.Monad (forever)
import Control.Concurrent.STM.TChan
import GHC.Conc

import Scurry.TapConfig
import Scurry.Types.Threads
import Scurry.Types.TAP

tapWriterThread :: TapWriterChan -> TapDesc -> IO ()
tapWriterThread c tap = forever $ do
    frame <- atomically $ readTChan c
    write_tap tap frame
