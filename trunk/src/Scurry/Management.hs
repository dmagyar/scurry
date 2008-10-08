module Scurry.Management where

import Network.Socket
import Text.JSON

data Scurry = Scurry VpnConfig NetworkConfig
data VpnConfig = VpnConfig DevIP DevMask
data NetworkConfig = NetworkConfig EndPoint

type DevIP = HostAddress
type DevMask = HostAddress

type EndPoint = SockAddr
