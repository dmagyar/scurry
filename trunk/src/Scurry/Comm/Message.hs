module Scurry.Comm.Message(
    FramePair,
    ScurryMsg(..),
) where

import Control.Monad (liftM,liftM2)
import Data.Binary
import qualified Data.ByteString as BSS
import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Scurry.Types

-- |These are the messages we get across the network.
-- They are the management and data protocol.
data ScurryMsg = SFrame FramePair       -- | An ethernet frame.
               | SJoin                  -- | A network join request.
               | SKeepAlive             -- | A keep alive message. 
               | SNotifyPeer SockAddr   -- | A message to notify others of a peer.
               | SRequestPeer           -- | A message to request peer listings on the network.
               | SUnknown               -- | An unknown message
    deriving (Show)

type FramePair = (EthernetHeader,BSS.ByteString)

instance Binary ScurryMsg where
    get = do tag <- getWord8
             case tag of
                  0 -> get >>= (return . SFrame)      -- SFrame
                  1 -> return SJoin                   -- SJoin
                  2 -> return SKeepAlive              -- SKeepAlive
                  3 -> get >>= (return . SNotifyPeer) -- SNotifyPeer
                  4 -> return SRequestPeer            -- SRequestPeer
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
                  -- 1 -> liftM4 SockAddrInet6 get get get get -- #Job removed, not compatable with windows
                  -- 2 -> liftM SockAddrUnix get
                  _ -> error "Not a SockAddr"
    put (SockAddrInet pn ha) =
        do putWord8 0
           put pn
           put ha
    -- #Job - removed, not compatable with windows
    -- put (SockAddrInet6 pn fi ha si) =
        -- do putWord8 1
           -- put pn
           -- put fi
           -- put ha
           -- put si
    -- put (SockAddrUnix s) =
        -- do putWord8 2
           -- put s

