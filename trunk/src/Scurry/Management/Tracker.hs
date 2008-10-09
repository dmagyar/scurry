module Scurry.Management.Tracker where

import Text.JSON
import Scurry.Util
import Network.Socket hiding (inet_addr,inet_ntoa)

type Tracker = [ScurryPeer]

data ScurryPeer = ScurryPeer HostAddress PortNumber

scurry_err :: Result a
scurry_err = Error "Not a Scurry Peer JSON object."

instance JSON ScurryPeer where
    readJSON (JSObject obj) = let objl = fromJSObject obj
                                  h = lookup "host" objl
                                  p = lookup "port" objl
                              in case (h,p) of
                                      (Just h',Just p') -> rj h' p'
                                      _ -> scurry_err
        where rj (JSString h) (JSRational p) = let h' = inet_addr $ fromJSString h
                                                   p' = truncate p
                                               in case h' of
                                                       (Just h'') -> Ok $ ScurryPeer h'' p'
                                                       _ -> scurry_err
              rj _ _ = scurry_err
    readJSON _ = scurry_err

    showJSON (ScurryPeer ha pn) = JSObject $ toJSObject o
        where o = [("host",JSString $ toJSString $ (\(Just v) -> v) $ inet_ntoa ha),
                   ("port",(JSRational . fromIntegral) pn)]
