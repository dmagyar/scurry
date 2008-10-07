module Main where

import System.Environment
import System.IO
import Control.Concurrent

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Scurry.TapConfig
import Scurry.Communication

main :: IO ()
main = do 
    (tapIp:myIp:yourIp:_) <- getArgs
    tap <- getTapHandle tapIp

    m <- inet_addr myIp
    y <- inet_addr yourIp

    case tap of
        (Left t)  -> putStrLn $ "Failed: " ++ (show t)
        (Right t) -> doWork t m y

doWork :: Handle -> HostAddress -> HostAddress -> IO ()
doWork tap myIp yourIp = do
    let sai = (SockAddrInet 24999 yourIp)

    local <- prepLocalEndPoint myIp 24999

    forkIO $ localProcessing tap local sai
    remoteProcessing tap local

