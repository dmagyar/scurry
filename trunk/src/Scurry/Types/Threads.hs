module Scurry.Types.Threads (
    SockWriterChan,
    ConMgrChan,
    TapWriterChan,
) where

import qualified Data.ByteString as BSS
import Control.Concurrent.STM.TChan

import Scurry.Comm.Util
import Scurry.Comm.Message
import Scurry.Types.Network

type SockWriterChan = TChan (DestAddr,ScurryMsg)
type ConMgrChan = TChan (EndPoint,ScurryMsg)
type TapWriterChan = TChan BSS.ByteString
