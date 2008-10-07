module Main where

import System.Environment
import System.IO
import Control.Concurrent

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Scurry.TapConfig
import Scurry.Communication

main :: IO ()
main = do 
    (tapIp:myIp:myPort:yourIp:yourPort:_) <- getArgs
    tap <- getTapHandle tapIp

    m <- inet_addr myIp
    y <- inet_addr yourIp

    let r = read :: (String -> Int)
        myAddr = SockAddrInet (fromIntegral . r $ myPort) m
        yourAddr = SockAddrInet (fromIntegral . r$ yourPort) y

    case tap of
        (Left t)  -> putStrLn $ "Failed: " ++ (show t)
        (Right t) -> doWork t myAddr yourAddr

doWork :: Handle -> SockAddr -> SockAddr -> IO ()
doWork tap myIp yourIp = do
    local <- prepEndPoint myIp

    -- Send a thread off to read from the TAP device
    forkIO $ localProcessing tap local yourIp

    -- This thread will read from the network socket
    remoteProcessing tap local

