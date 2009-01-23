module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import Network.Socket (withSocketsDo)
import Network.BSD

import Scurry.Peer
import Scurry.Comm
-- import Scurry.Management.Config
-- import Scurry.Management.Tracker
import Scurry.State
import Scurry.Util(inet_addr)
import Scurry.Network
import Scurry.Types.Network
import Data.Word
import Data.Maybe

data ScurryOpts = ScurryOpts {
    vpnAddr  :: Maybe ScurryAddress,
    vpnMask  :: Maybe ScurryMask,
    bindAddr :: ScurryAddress,
    bindPort :: ScurryPort,
    peers    :: [EndPoint]
} deriving (Show)

data ScurryOpt = SOVAddr ScurryAddress
               | SOVMask ScurryMask
               | SOBAddr ScurryAddress
               | SOBPort ScurryPort
    deriving (Show)

defaultPort :: ScurryPort
defaultPort = ScurryPort 24999

readPort :: String -> ScurryPort
readPort [] = error "I need some numbers!"
readPort s = let w = read s :: Word16
             in (ScurryPort . fromIntegral) w

lookupName :: String -> IO EndPoint
lookupName ep = do
    let name = takeWhile (/=':') ep
        port = case drop 1 $ dropWhile (/=':') ep of
                    [] -> defaultPort
                    x  -> readPort x

    HostEntry { hostAddresses = as } <- getHostByName name

    return $ EndPoint (ScurryAddress (as !! 0)) port
          
parseScurryOpts :: [String] -> IO (Either String ScurryOpts)
parseScurryOpts args = let def_opt = ScurryOpts { vpnAddr = Nothing
                                                , vpnMask = Nothing
                                                , bindAddr = ScurryAddress 0
                                                , bindPort = ScurryPort 24999
                                                , peers = [] }
                           va_t = SOVAddr . fromJust . inet_addr
                           vm_t = SOVMask . fromJust . inet_addr
                           ba_t = SOBAddr . fromJust . inet_addr
                           bp_t = SOBPort . readPort
                           options = [ Option "a" ["vpn-addr"]  (ReqArg va_t "VPN_ADDRESS")  "The IP address to use for the VPN."
                                     , Option "m" ["vpn-mask"]  (ReqArg vm_t "VPN_NETMASK")  "The network mask the for the VPN."
                                     , Option "b" ["bind-addr"] (ReqArg ba_t "BIND_ADDRESS") "The network interface on which listen for connections."
                                     , Option "p" ["bind-port"] (ReqArg bp_t "BIND_PORT")    "The port on which to listen for connections." ]
                           (m,o,_) = getOpt RequireOrder options args -- ignore errors for now
                           u = Left (usageInfo "How to use scurry." options)
                           opt' = foldr rplcOpts def_opt m
                       in case args of
                               ["-h"]     -> return u
                               ["--help"] -> return u
                               _          -> do ps <- mapM lookupName o
                                                return $ Right (opt' { peers = ps })

    where rplcOpts (SOVAddr a) sos = sos { vpnAddr  = Just a }
          rplcOpts (SOVMask m) sos = sos { vpnMask  = Just m }
          rplcOpts (SOBAddr a) sos = sos { bindAddr = a }
          rplcOpts (SOBPort p) sos = sos { bindPort = p }

main :: IO ()
main = withSocketsDo $ do 
    args <- getArgs

    opts_ <- parseScurryOpts args
    -- let (configPath:trackerPath:_) = args

    opts <- case opts_ of
                 (Left msg) -> putStrLn msg >> exitFailure
                 (Right p)  -> print p >> return p

    -- (Just config)  <- load_scurry_config_file configPath
    -- (Just tracker) <- load_tracker_file trackerPath

    -- let (Scurry (VpnConfig tapIp tapMask) (NetworkConfig mySockAddr)) = config
    --     trackerEndPoints = filter (/= mySockAddr) $ map tToS tracker

    let initState = ScurryState {
            scurryPeers = [],
            scurryEndPoint = EndPoint (bindAddr opts) (bindPort opts),
            scurryNetwork = ScurryNetwork {
                scurryMask = vpnMask opts
            },
            scurryMyRecord = PeerRecord {
                peerMAC = Nothing,
                peerEndPoint = EndPoint (ScurryAddress 0) (ScurryPort 0),
                peerVPNAddr = vpnAddr opts,
                peerLocalPort = bindPort opts -- (\(EndPoint _ p) -> p) mySockAddr
            }
        }
        tapNet = (vpnAddr opts, vpnMask opts)

    local <- prepEndPoint (scurryEndPoint initState) -- mySockAddr

    startCom tapNet local initState (peers opts)

    {-
    where
        tToS (ScurryPeer ip port) = EndPoint ip port 
    -}


{-
getAddress :: Socket -> [EndPoint] -> IO (ScurryAddress, ScurryMask)
getAddress sock (t:trackers) = do
    let msg = SAddrRequest
        cmd = sendToAddr sock msg
    
    putStrLn "Requesting address..."
    cmd t

    where
        delayRead = do
            threadDelay (sToMs 1.5)
-}            
