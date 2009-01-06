module Scurry.Comm.SockWrite (
sockWriteThread,
sendToAddr,
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
import Scurry.Types.Network
import Scurry.Types.Threads

sockWriteThread :: Socket -> SockWriterChan -> IO ()
sockWriteThread sock = forever . sockWriter sock

sockWriter :: Socket -> SockWriterChan -> IO ()
sockWriter sock chan = do
    (dst,msg) <- atomically $ readTChan chan
    
    case dst of
         DestSingle addr -> (sendToAddr sock msg addr) >> return ()
         DestList addrs -> mapM_ (sendToAddr sock msg) addrs


sendToAddr :: Socket -> ScurryMsg -> EndPoint -> IO Int
sendToAddr s m = sendTo s (BSS.concat . BS.toChunks $ encode m) . epToSa
