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
import Control.Concurrent.STM.TChan
import Data.IORef
import GHC.Conc

import Scurry.Ethernet

readLength :: Int
readLength = 1500

data ScurryState = ScurryState [SockAddr]

data DestAddr = DestSingle SockAddr
              | DestList [SockAddr]
    deriving (Show)

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
    
-- |Bind the socket to the specified socket address.
-- This specifies the network configuration we're using
-- as well.
prepEndPoint :: SockAddr -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s ep
    return s

-- |Reads packets off from the local TAP device
-- and then writes them to the network sockets
-- provided.
localProcessing :: Handle -> Socket -> [SockAddr] -> IO ()
localProcessing tap net ha = forever $ do
    (readTAP tap) >>= (writeNet net ha)

-- |Reads packets off from the network socket
-- and writes them to the TAP device.
remoteProcessing :: Handle -> Socket -> IO ()
remoteProcessing tap net = forever $ do
    (readNet net) >>= (writeTAP tap)

-- |Takes an ethernet frame pair and prints some debug
-- information about it.
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



{-
 - 1. Application Start
 - 2. Setup TAP device
 - 3. Create network socket
 - 4. Create network reading routine
 - 5. Create TAP reading routine
 - 6. Create network writing routine (on MVar)
 - 7. Create TAP writing routine (on MVar)
 -
 - Network reader accepts packets, decodes their type,
 - and then decides what to do with them.
 -
 - TAP reading thread encodes the read frame
 - and then forwards it onto the frame switch.
 -
 - Network writer waits on an MVar of type (SockAddr,ByteString)
 - and writes the string to the provided address.
 -
 - The TAP reader waits on an MVar of type (ByteString) and
 - writes the string to the TAP device.
 -
 - The frame switch decides where a packet is going and calls
 - the necessary writeNet call.
 -
 -}

-- |A thread which synchronizes with the switch thread
-- to read packets from the TAP device and send them 
-- to the world.

tapSourceThread :: Handle -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
tapSourceThread tap ssRef chan = forever $
    tapReader tap >>=
    (\x -> frameSwitch ssRef chan (tapDecode x))

sockWriteThread :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriteThread sock chan = forever $
    sockWriter sock chan

sockSourceThread :: Handle -> Socket -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockSourceThread tap sock ssRef chan = forever $
    (sockReader sock) >>=
    (\(addr,msg) -> routeInfo tap ssRef (addr,sockDecode msg))
    

-- tapReader :: (IORef ScurryState) -> 
tapReader tap = do
    hWaitForInput tap (-1)
    BS.hGetNonBlocking tap readLength

tapDecode :: BS.ByteString -> ScurryMsg
tapDecode bs = SFrame $ bsToEthernetTuple bs

-- frameSwitch :: ScurryMsg -> IO 
frameSwitch :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> ScurryMsg -> IO ()
frameSwitch ssRef chan m = do
    (ScurryState peers) <- readIORef ssRef
    mapM_ (\x -> atomically $ writeTChan chan (DestSingle x,m)) peers

-- consoleSVC :: (IORef ScurryState) ->
-- keepAliveSVC :: (IORef ScurryState) ->

sockWriter :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriter sock chan = do
    (dst,msg) <- atomically $ readTChan chan
    case dst of
         DestSingle addr -> sendTo sock (BSS.concat . BS.toChunks $ encode msg) addr >> return ()
         _ -> error "Error: I don't support anything except DestSingle yet."

sockReader :: Socket -> IO (SockAddr,BS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (addr,BS.fromChunks [msg])

sockDecode :: BS.ByteString -> ScurryMsg
sockDecode msg = decode msg

routeInfo :: Handle -> (IORef ScurryState) -> (SockAddr,ScurryMsg) -> IO ()
routeInfo tap ssRef (srcAddr,msg) = do
    case msg of
         SFrame (hdr,frame) -> tapWriter tap frame
         _ -> error "Only SFrame messages are supported at this time."

tapWriter :: Handle -> BS.ByteString -> IO ()
tapWriter tap frame = do
    BSS.hPut tap (BSS.concat . BS.toChunks $ frame)
    hFlush tap
