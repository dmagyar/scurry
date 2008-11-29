module Scurry.Management.Config(
    Scurry(..),
    VpnConfig(..),
    NetworkConfig(..),
    DevIP,
    DevMask,
    EndPoint,
    load_scurry_config_file
) where

import Scurry.Util

import Text.JSON

import Scurry.Types.Network

data Scurry = Scurry VpnConfig NetworkConfig
    deriving (Show)

data VpnConfig = VpnConfig DevIP DevMask
    deriving (Show)

data NetworkConfig = NetworkConfig EndPoint
    deriving (Show)

type DevIP = ScurryAddress
type DevMask = ScurryAddress

load_scurry_config_file :: FilePath -> IO (Maybe Scurry)
load_scurry_config_file file = do f <- readFile file
                                  return $ case (decode f) of
                                                Ok f'   -> Just f'
                                                Error _ -> Nothing

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
    showJSON (NetworkConfig (EndPoint host (ScurryPort port))) =
        let (Just host') = inet_ntoa host
        in JSObject $ toJSObject [("host",JSString $ toJSString host'),
                                  ("port",JSRational (fromIntegral port))]
    
    readJSON (JSObject obj) = let objl = fromJSObject obj
                                  host = lookup "host" objl
                                  port = lookup "port" objl
                              in case (host,port) of
                                      (Just h,Just p) -> rj h p
                                      _ -> net_err
        where rj (JSString h) (JSRational p) =
                let h' = inet_addr $ fromJSString h
                in case h' of
                        (Just h'') -> Ok $ NetworkConfig $ EndPoint h'' (ScurryPort (truncate p))
                        Nothing -> net_err
              rj _ _ = net_err
    readJSON _ = net_err
