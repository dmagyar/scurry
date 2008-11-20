module Scurry.Types.Threads (
    SockWriterChan,
    ConMgrChan,
) where

import Scurry.Comm.Util
import Scurry.Comm.Message
import Scurry.Types.Network

import Control.Concurrent.STM.TChan

type SockWriterChan = TChan (DestAddr,ScurryMsg)
type ConMgrChan = TChan (EndPoint,ScurryMsg)
