module Scurry.Communication(
    prepLocalEndPoint,
    prepRemoteEndPoint,
    remoteProcessing,
    localProcessing,
    debugFrame
) where


import System.IO
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad (forever)

import Scurry.Ethernet

prepRemoteEndPoint :: IO Socket
prepRemoteEndPoint = do
    socket AF_INET Datagram defaultProtocol

prepLocalEndPoint :: HostAddress -> PortNumber -> IO Socket
prepLocalEndPoint ep port = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s (SockAddrInet port ep)
    return s

localProcessing :: Handle -> Socket -> SockAddr -> IO ()
localProcessing tap net ha = forever $ do
    (readTAP tap) >>= (writeNet net ha)

remoteProcessing :: Handle -> Socket -> IO ()
remoteProcessing tap net = forever $ do
    (readNet net) >>= (writeTAP tap)

-- |Some functions to do the reading/writing
debugFrame :: (EthernetHeader,BS.ByteString) -> IO ()
debugFrame (h,f) = do
    putStrLn $ (show h) ++ " => Length: " ++ (show $ BS.length f)

readTAP :: Handle -> IO (EthernetHeader,BS.ByteString)
readTAP tap = do
    hWaitForInput tap (-1)

    d <- BS.hGetNonBlocking tap 1500
    let et = bsToEthernetTuple d

    return et

writeTAP :: Handle -> (EthernetHeader,BS.ByteString) -> IO ()
writeTAP tap (_,frame) = do
    let ef = frame

    BSS.hPut tap (BSS.concat . BS.toChunks $ ef)
    hFlush tap

readNet :: Socket -> IO (EthernetHeader,BS.ByteString)
readNet net = do
    (msg,_) <- recvFrom net 1500 
    let frame = bsToEthernetTuple $ BS.fromChunks [msg]

    return frame

writeNet :: Socket -> SockAddr -> (EthernetHeader,BS.ByteString) -> IO ()
writeNet net ha (_,frame) = do
    _ <- sendTo net (BSS.concat . BS.toChunks $ frame) ha
    return ()

bsToEthernetTuple :: BS.ByteString -> (EthernetHeader,BS.ByteString)
bsToEthernetTuple d = (decode d, d)
