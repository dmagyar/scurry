module Scurry.Communication(
    prepEndPoint,
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
import Control.Monad (forever,liftM,liftM2,liftM4)

import Scurry.Ethernet

readLength :: Int
readLength = 1500

type FramePair = (EthernetHeader,BS.ByteString)

-- |These are the messages we get across the network.
-- They are the management and data protocol.
data ScurryMsg = SFrame FramePair       -- | An ethernet frame.
               | SJoin                  -- | A network join request.
               | SKeepAlive             -- | A keep alive message. 
               | SNotifyPeer SockAddr   -- | A message to notify others of a peer.
               | SRequestPeer           -- | A message to request peer listings on the network.
               | SUnknown               -- | An unknown message
    deriving (Show)

instance Binary ScurryMsg where
    get = do tag <- getWord8
             case tag of
                  0 -> get >>= (return . SFrame)      -- SFrame
                  1 -> return SJoin                   -- SJoin
                  2 -> get >>= (return . SNotifyPeer) -- SNotifyPeer
                  3 -> return SRequestPeer            -- SRequestPeer
                  _ -> return SUnknown                -- Unknown Message
    
    put (SFrame fp) = do putWord8 0
                         put fp
    put SJoin = putWord8 1
    put (SNotifyPeer p) = do putWord8 2
                             put p
    put SRequestPeer = putWord8 3
    put SUnknown = putWord8 255

instance Binary PortNumber where
    get = liftM PortNum get
    put (PortNum p) = put p

instance Binary SockAddr where
    get = do tag <- getWord8
             case tag of
                  0 -> liftM2 SockAddrInet get get
                  1 -> liftM4 SockAddrInet6 get get get get
                  2 -> liftM SockAddrUnix get
                  _ -> error "Not a SockAddr"
    put (SockAddrInet pn ha) = do putWord8 0
                                  put pn
                                  put ha
    put (SockAddrInet6 pn fi ha si) =  do putWord8 1
                                          put pn
                                          put fi
                                          put ha
                                          put si
    put (SockAddrUnix s) = do putWord8 2
                              put s
    
prepEndPoint :: SockAddr -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s ep
    return s

localProcessing :: Handle -> Socket -> [SockAddr] -> IO ()
localProcessing tap net ha = forever $ do
    (readTAP tap) >>= (writeNet net ha)

remoteProcessing :: Handle -> Socket -> IO ()
remoteProcessing tap net = forever $ do
    (readNet net) >>= (writeTAP tap)

-- |Some functions to do the reading/writing
debugFrame :: (EthernetHeader,BS.ByteString) -> IO ()
debugFrame (h,f) = do
    putStrLn $ concat [(show h)," => Length: ",(show $ BS.length f)]

readTAP :: Handle -> IO (EthernetHeader,BS.ByteString)
readTAP tap = do
    hWaitForInput tap (-1)
    (BS.hGetNonBlocking tap readLength) >>= (return . bsToEthernetTuple)

writeTAP :: Handle -> (EthernetHeader,BS.ByteString) -> IO ()
writeTAP tap (_,frame) = do
    BSS.hPut tap (BSS.concat . BS.toChunks $ frame)
    hFlush tap

readNet :: Socket -> IO (EthernetHeader,BS.ByteString)
readNet net = do
    (msg,_) <- recvFrom net readLength 
    return . bsToEthernetTuple . BS.fromChunks $ [msg]

writeNet :: Socket -> [SockAddr] -> (EthernetHeader,BS.ByteString) -> IO ()
writeNet net has (_,frame) = do
    _ <- mapM (sendTo net (BSS.concat . BS.toChunks $ frame)) has 
    return ()

bsToEthernetTuple :: BS.ByteString -> (EthernetHeader,BS.ByteString)
bsToEthernetTuple d = (decode d, d)
