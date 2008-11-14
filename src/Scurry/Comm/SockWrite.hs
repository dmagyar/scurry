module Scurry.Comm.SockWrite (
sockWriteThread
) where

import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.Binary
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import GHC.Conc
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Scurry.Comm.Util
import Scurry.Comm.Message

sockWriteThread :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriteThread sock chan = forever $
    sockWriter sock chan

sockWriter :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriter sock chan = do
    (dst,msg) <- atomically $ readTChan chan
    
    let sendToAddr = sendTo sock (BSS.concat . BS.toChunks $ encode msg)

    case dst of
         DestSingle addr -> sendToAddr addr >> return ()
         DestList addrs -> mapM_ sendToAddr addrs

