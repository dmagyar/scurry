
module Scurry.Util(
    catch_to_maybe,
    inet_addr,
    inet_ntoa,
    sToMs,
    networkLowAddress,
    networkHighAddress,
    enumAllInMask,
    genRandAddr,
    isInMask,
) where

import qualified Network.Socket as INET (inet_addr,inet_ntoa)

import Data.Time
import System.Random
import System.IO.Unsafe (unsafePerformIO)

import Scurry.Types.Network
import Data.Bits

import Network.Util

{- I really don't like that inet_addr/inet_ntoa need to
 - run in IO. I also don't like how they throw errors
 - around instead of Nothing. I've fixed both problems! -}
catch_to_maybe :: (t -> IO a) -> t -> IO (Maybe a)
catch_to_maybe f a = catch (f a >>= (return . Just)) (\_ -> return Nothing)

inet_addr :: String -> Maybe ScurryAddress
inet_addr a = unsafePerformIO $ catch_to_maybe (\v -> (INET.inet_addr v) >>= (return . ScurryAddress)) a

inet_ntoa :: ScurryAddress -> Maybe String
inet_ntoa (ScurryAddress a) = unsafePerformIO $ catch_to_maybe INET.inet_ntoa a

-- | Seconds to milliseconds
sToMs :: Int -> Int
sToMs = (* 1000000)

genRandAddr :: [ScurryAddress] -> ScurryAddress -> ScurryAddress -> IO ScurryAddress
genRandAddr without mask net = do
    ct <- getCurrentTime

    let seed = 1000000000 * (diffUTCTime ct (UTCTime (ModifiedJulianDay 0) 0))
        gen = mkStdGen $ round seed

    let (r,_) = randomR (0,100) gen
        a = (!! r) $ filter (not . (flip elem without)) $ enumAllInMask mask net

    return a
    

-- | Network utility functions

isInMask :: ScurryMask -> ScurryAddress -> ScurryAddress -> Bool
isInMask mask net addr = let m = scurryAddr mask
                             n = m .&. (scurryAddr net)
                             a = scurryAddr addr
                         in n == a
enumAllInMask :: ScurryMask -> ScurryAddress -> [ScurryAddress]
enumAllInMask mask net = let l = networkLowAddress mask net
                             h = networkHighAddress mask net
                         in [l..h]

networkLowAddress :: ScurryMask -> ScurryAddress -> ScurryAddress
networkLowAddress mask addr = let a = (ntohl . scurryAddr) addr
                                  m = (ntohl . scurryAddr) mask
                                  n = a .&. m
                              in ScurryAddress $ htonl (n + 1)

networkHighAddress :: ScurryMask -> ScurryAddress -> ScurryAddress
networkHighAddress mask addr = let m = (ntohl . scurryAddr) mask
                                   a = (ntohl . scurryAddr) addr
                                   c = complement m
                                   n = (m .&. a) + c
                               in ScurryAddress (htonl (n - 1)) -- Highest address is broadcast
