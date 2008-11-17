module Scurry.Comm.Message(
    FramePair,
    ScurryMsg(..),
) where

import Control.Monad (liftM,liftM2)
import Data.Binary
import qualified Data.ByteString as BSS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Data.Word

import Scurry.Types

type PingID = Word32

-- |These are the messages we get across the network.
-- They are the management and data protocol.
data ScurryMsg = SFrame FramePair              -- | An ethernet frame.
               | SJoin MACAddr                 -- | A network join request.
               | SJoinReply MACAddr [SockAddr] -- | A network join reply.
               | SKeepAlive                    -- | A keep alive message. 
               | SNotifyPeer SockAddr          -- | A message to notify others of a peer.
               | SRequestPeer                  -- | A message to request peer listings on the network.
               | SPing PingID                  -- | A Ping command used for diagnostics.
               | SEcho PingID                  -- | A Echo command used to respond to the Ping command.
               | SUnknown                      -- | An unknown message
    deriving (Show)

type FramePair = (EthernetHeader,BSS.ByteString)

instance Binary ScurryMsg where
    get = do tag <- getWord8
             case tag of
                  0 -> get >>= (return . SFrame)      -- SFrame
                  1 -> get >>= (return . SJoin)       -- SJoin
                  2 -> liftM2 SJoinReply get get  -- SJoinReply
                  3 -> return SKeepAlive              -- SKeepAlive
                  4 -> get >>= (return . SNotifyPeer) -- SNotifyPeer
                  5 -> return SRequestPeer            -- SRequestPeer
                  6 -> get >>= (return . SPing)
                  7 -> get >>= (return . SEcho)
                  _ -> return SUnknown                -- Unknown Message
    
    put (SFrame fp)      = putWord8 0 >> put fp
    put (SJoin m)        = putWord8 1 >> put m
    put (SJoinReply m p) = putWord8 2 >> put m >> put p
    put SKeepAlive       = putWord8 3
    put (SNotifyPeer p)  = putWord8 4 >> put p
    put SRequestPeer     = putWord8 5
    put (SPing pp)       = putWord8 6 >> put pp
    put (SEcho pe)       = putWord8 7 >> put pe
    put SUnknown         = putWord8 255

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

