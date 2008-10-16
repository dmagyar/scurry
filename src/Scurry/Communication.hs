module Scurry.Communication(
    prepEndPoint,
    debugFrame,

    startCom,

    ScurryState(..)
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
    
    put (SFrame fp)     = putWord8 0 >> put fp
    put SJoin           = putWord8 1
    put SKeepAlive      = putWord8 2
    put (SNotifyPeer p) = putWord8 3 >> put p
    put SRequestPeer    = putWord8 4
    put SUnknown        = putWord8 255

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
-- This specifies the network configuration we are using
-- as well.
prepEndPoint :: SockAddr -> IO Socket
prepEndPoint ep = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s ep
    return s

-- |Takes an ethernet frame pair and prints some debug
-- information about it.
debugFrame :: (EthernetHeader,BS.ByteString) -> IO ()
debugFrame (h,f) = do
    putStrLn $ concat [(show h)," => Length: ",(show $ BS.length f)]


bsToEthernetTuple :: BS.ByteString -> (EthernetHeader,BS.ByteString)
bsToEthernetTuple d = (decode d, d)


startCom :: Handle -> Socket -> ScurryState -> IO ()
startCom tap sock initSS = do
    ssRef <- newIORef initSS
    chan  <- atomically $ newTChan

    forkIO $ tapSourceThread tap ssRef chan
    forkIO $ sockWriteThread sock chan
    sockSourceThread tap sock ssRef
    

tapSourceThread :: Handle -> (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> IO ()
tapSourceThread tap ssRef chan = forever $
    tapReader tap >>=
    (\x -> frameSwitch ssRef chan (tapDecode x))

sockWriteThread :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriteThread sock chan = forever $
    sockWriter sock chan

sockSourceThread :: Handle -> Socket -> (IORef ScurryState) -> IO ()
sockSourceThread tap sock ssRef = forever $
    (sockReader sock) >>=
    (\(addr,msg) -> routeInfo tap ssRef (addr,sockDecode msg))
    
tapReader :: Handle -> IO BS.ByteString
tapReader tap = do
    hWaitForInput tap (-1)
    BS.hGetNonBlocking tap readLength

tapDecode :: BS.ByteString -> ScurryMsg
tapDecode bs = SFrame $ bsToEthernetTuple bs

frameSwitch :: (IORef ScurryState) -> (TChan (DestAddr,ScurryMsg)) -> ScurryMsg -> IO ()
frameSwitch ssRef chan m = do
    (ScurryState peers) <- readIORef ssRef
    mapM_ (\x -> atomically $ writeTChan chan (DestSingle x,m)) peers

sockWriter :: Socket -> (TChan (DestAddr,ScurryMsg)) -> IO ()
sockWriter sock chan = do
    (dst,msg) <- atomically $ readTChan chan
    
    let sendToAddr = sendTo sock (BSS.concat . BS.toChunks $ encode msg)

    case dst of
         DestSingle addr -> sendToAddr addr >> return ()
         DestList addrs -> mapM_ sendToAddr addrs

sockReader :: Socket -> IO (SockAddr,BS.ByteString)
sockReader sock = do
    (msg,addr) <- recvFrom sock readLength
    return (addr,BS.fromChunks [msg])

sockDecode :: BS.ByteString -> ScurryMsg
sockDecode msg = decode msg

routeInfo :: Handle -> (IORef ScurryState) -> (SockAddr,ScurryMsg) -> IO ()
routeInfo tap ssRef (srcAddr,msg) = do
    case msg of
         SFrame (_,frame) -> tapWriter tap frame
         SJoin            -> atomicModifyIORef ssRef updatePeers
         SKeepAlive       -> error "SKeepAlive not supported"
         SNotifyPeer _    -> error "SNotifyPeer not supported"
         SRequestPeer     -> error "SRequestPeer not supported"
         SUnknown         -> error "SUnknown not supported"
    where updatePeers ss@(ScurryState ps) = if elem srcAddr ps
                                               then (ss,())
                                               else (ScurryState $ srcAddr : ps,())

tapWriter :: Handle -> BS.ByteString -> IO ()
tapWriter tap frame = do
    BSS.hPut tap (BSS.concat . BS.toChunks $ frame)
    hFlush tap
