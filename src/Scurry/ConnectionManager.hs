module Scurry.ConnectionManager (
    conMgrThread,
) where

import Scurry.State
import Scurry.Types.Threads
import Control.Monad (forever)
import GHC.Conc
import Scurry.Util

conMgrThread :: StateRef -> SockWriterChan -> ConMgrChan -> IO ()
conMgrThread sr swc cmc = forever $ do
    threadDelay (msToS 5)
