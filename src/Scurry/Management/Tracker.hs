module Scurry.Management.Tracker where

import Text.JSON
import Scurry.Util

import Scurry.Types.Network

type Tracker = [ScurryPeer]

data ScurryPeer = ScurryPeer ScurryAddress ScurryPort
    deriving (Show)

scurry_err :: Result a
scurry_err = Error "Not a Scurry Peer JSON object."

load_tracker_file :: FilePath -> IO (Maybe Tracker)
load_tracker_file path = do f <- readFile path
                            return $ case decode f of
                                          Ok f'   -> Just f'
                                          Error _ -> Nothing

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
                                                       (Just h'') -> Ok $ ScurryPeer h'' (ScurryPort p')
                                                       _ -> scurry_err
              rj _ _ = scurry_err
    readJSON _ = scurry_err

    showJSON (ScurryPeer ha (ScurryPort pn)) = JSObject $ toJSObject o
        where o = [("host",JSString $ toJSString $ (\(Just v) -> v) $ inet_ntoa ha),
                   ("port",(JSRational . fromIntegral) pn)]
