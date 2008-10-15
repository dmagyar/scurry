module Scurry.Util(
    catch_to_maybe,
    inet_addr,
    inet_ntoa
) where

import Network.Socket hiding (inet_addr,inet_ntoa)
import qualified Network.Socket as INET (inet_addr,inet_ntoa)
import System.IO.Unsafe (unsafePerformIO)

{- I really don't like that inet_addr/inet_ntoa need to
 - run in IO. I also don't like how they throw errors
 - around instead of Nothing. I've fixed both problems! -}
catch_to_maybe :: (t -> IO a) -> t -> IO (Maybe a)
catch_to_maybe f a = catch (f a >>= (return . Just)) (\_ -> return Nothing)

inet_addr :: String -> Maybe HostAddress
inet_addr a = unsafePerformIO $ catch_to_maybe INET.inet_addr a

inet_ntoa :: HostAddress -> Maybe String
inet_ntoa a = unsafePerformIO $ catch_to_maybe INET.inet_ntoa a


