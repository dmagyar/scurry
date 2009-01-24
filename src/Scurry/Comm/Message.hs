module Scurry.Comm.Message(
    ScurryMsg(..),
) where

import Control.Monad
import Data.Binary
import qualified Data.ByteString as BSS
import Data.Word

import Scurry.Types.Network
import Scurry.Peer

type PingID = Word32

-- |These are the messages we get across the network.
-- They are the management and data protocol.
data ScurryMsg = SFrame BSS.ByteString            -- | An ethernet frame.
               | SJoin PeerRecord                 -- | A network join request.
               | SJoinReply PeerRecord [EndPoint] -- | A network join reply.
               | SKeepAlive PeerRecord            -- | A keep alive message. 
               | SNotifyPeer EndPoint             -- | A message to notify others of a peer.
               | SRequestPeer                     -- | A message to request peer listings on the network.
               | SPing PingID                     -- | A Ping command used for diagnostics.
               | SEcho PingID                     -- | A Echo command used to respond to the Ping command.
               | SLANProbe                        -- | A message to probe the local LAN for other members.
               | SLANSuggest ScurryPort           -- | A message to inform a peer that they may share a LAN with another.
               | SAddrRequest                     -- | A message to request an available VPN address
               -- | SAddrReject                      -- | A message to reject the address a peer has chosen
               | SAddrPropose ScurryAddress ScurryMask -- | A message to suggest an address to a peer
               -- | SAddrSelect ScurryAddress        -- | A message to inform every one we're using an address 
               | SUnknown                         -- | An unknown message
    deriving (Show)

instance Binary ScurryMsg where
    get = do tag <- getWord8
             case tag of
                  0  -> liftM SFrame get          -- SFrame
                  1  -> liftM SJoin  get          -- SJoin
                  2  -> liftM2 SJoinReply get get -- SJoinReply
                  3  -> liftM SKeepAlive get      -- SKeepAlive
                  4  -> liftM SNotifyPeer get     -- SNotifyPeer
                  5  -> return SRequestPeer       -- SRequestPeer
                  6  -> liftM SPing get           -- SPing
                  7  -> liftM SEcho get           -- SEcho
                  8  -> return SLANProbe          -- SLANProbe
                  9  -> liftM SLANSuggest get     -- SLANSuggest
                  10 -> return SAddrRequest       -- SAddrRequest
                  -- 11 -> return SAddrReject        -- SAddrReject
                  12 -> liftM2 SAddrPropose get get  -- SAddrPropose
                  -- 13 -> get >>= (return . SAddrSelect)  -- SAddrSelect
                  _  -> return SUnknown           -- Unknown Message
    
    put (SFrame fp)         = putWord8 0 >> put fp
    put (SJoin m)           = putWord8 1 >> put m
    put (SJoinReply m p)    = putWord8 2 >> put m >> put p
    put (SKeepAlive r)      = putWord8 3 >> put r
    put (SNotifyPeer p)     = putWord8 4 >> put p
    put SRequestPeer        = putWord8 5
    put (SPing pp)          = putWord8 6 >> put pp
    put (SEcho pe)          = putWord8 7 >> put pe
    put SLANProbe           = putWord8 8
    put (SLANSuggest ps)    = putWord8 9 >> put ps
    put SAddrRequest        = putWord8 10
    -- put SAddrReject         = putWord8 11
    put (SAddrPropose a m)    = putWord8 12 >> put a >> put m
    -- put (SAddrSelect p)     = putWord8 13 >> put p
    put SUnknown            = putWord8 255


