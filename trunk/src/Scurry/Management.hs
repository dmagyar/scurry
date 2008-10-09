module Scurry.Management(
    Scurry(..),
    VpnConfig(..),
    NetworkConfig(..),
    DevIP,
    DevMask,
    EndPoint,
) where

import Network.Socket hiding (inet_addr,inet_ntoa)
import qualified Network.Socket as INET (inet_addr,inet_ntoa)
import Text.JSON
import System.IO.Unsafe (unsafePerformIO)

data Scurry = Scurry VpnConfig NetworkConfig
    deriving (Show)

data VpnConfig = VpnConfig DevIP DevMask
    deriving (Show)

data NetworkConfig = NetworkConfig EndPoint
    deriving (Show)

type DevIP = HostAddress
type DevMask = HostAddress

type EndPoint = SockAddr

{- I really don't like that inet_addr/inet_ntoa need to
 - run in IO. I also don't like how they throw errors
 - around instead of Nothing. I've fixed both problems! -}
catch_to_maybe :: (t -> IO a) -> t -> IO (Maybe a)
catch_to_maybe f a = catch (f a >>= (return . Just)) (\_ -> return Nothing)

inet_addr :: String -> Maybe HostAddress
inet_addr a = unsafePerformIO $ catch_to_maybe INET.inet_addr a

inet_ntoa :: HostAddress -> Maybe String
inet_ntoa a = unsafePerformIO $ catch_to_maybe INET.inet_ntoa a


scurry_err, vpn_err, net_err :: Result a
scurry_err = Error "Not a Scurry JSON object."
vpn_err = Error "Not a Scurry VPN Config JSON object."
net_err = Error "Not a Scurry Network Config JSON object."

instance JSON Scurry where
    readJSON (JSObject obj) = let objl = fromJSObject obj
                                  vc = lookup "vpn" objl
                                  nc = lookup "network" objl
                              in case (vc,nc) of
                                      (Just v , Just n)  -> rj v n
                                      _ -> scurry_err
        where rj v n = let (v',n') = (readJSON v, readJSON n)
                       in case (v',n') of
                               (Ok v'', Ok n'') -> Ok $ Scurry v'' n''
                               _ -> scurry_err
    readJSON _ = scurry_err

    showJSON (Scurry vc nc) = JSObject $ toJSObject [("vpn",showJSON vc),
                                                     ("network",showJSON nc)]

instance JSON VpnConfig where
    showJSON (VpnConfig ip mask) = let (Just ip')   = inet_ntoa ip
                                       (Just mask') = inet_ntoa mask
                                   in rj ip' mask'
        where rj i m = JSObject $ toJSObject [("ip",JSString $ toJSString i),
                                              ("mask",JSString $ toJSString m)]

    readJSON (JSObject obj) = let objl = fromJSObject obj
                                  ip = lookup "ip" objl
                                  mask = lookup "mask" objl
                              in case (ip,mask) of
                                      (Just i,Just m) -> rj i m
                                      _ -> vpn_err
        where rj (JSString i) (JSString m) = 
                    let i' = inet_addr (fromJSString i)
                        m' = inet_addr (fromJSString m)
                    in case (i',m') of
                            (Just i'',Just m'') -> Ok $ VpnConfig i'' m''
                            _ -> vpn_err
              rj _ _ = vpn_err
    readJSON _ = vpn_err

instance JSON NetworkConfig where
    showJSON (NetworkConfig (SockAddrInet port host)) =
        let (Just host') = inet_ntoa host
        in JSObject $ toJSObject [("host",JSString $ toJSString host'),
                                  ("port",JSRational (fromIntegral port))]
    showJSON (NetworkConfig _) = error $ "Scurry doesn't support anything besides IPv4 yet. Sorry. :("
    
    readJSON (JSObject obj) = let objl = fromJSObject obj
                                  host = lookup "host" objl
                                  port = lookup "port" objl
                              in case (host,port) of
                                      (Just h,Just p) -> rj h p
                                      _ -> net_err
        where rj (JSString h) (JSRational p) =
                let h' = inet_addr $ fromJSString h
                in case h' of
                        (Just h'') -> Ok $ NetworkConfig $ SockAddrInet (truncate p) h''
                        Nothing -> net_err
              rj _ _ = net_err
    readJSON _ = net_err
