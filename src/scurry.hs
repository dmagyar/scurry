module Main where

import System.Console.GetOpt
import System.Directory
import System.Environment

import Network.Socket
import OpenSSL
import OpenSSL.RSA
import OpenSSL.PEM
import OpenSSL.EVP.PKey

appName :: String
appName = "scurry"

-- | Functional 'if'. I can use this with liftM now.
fif :: Bool -> a -> a -> a
fif p t f = if p then t
                 else f

-- | Returns a path to the user data directory
-- scurry uses to save persistant data like keys
-- and peer maps.
getDataDir :: IO FilePath
getDataDir = do
    p <- getAppUserDataDirectory appName
    createDirectoryIfMissing True p
    return p

pubKeyPath :: FilePath -> FilePath
pubKeyPath dd = dd ++ "/key.pub"

prvKeyPath :: FilePath -> FilePath
prvKeyPath dd = dd ++ "/key.prv"

getKey :: FilePath -> IO RSAKeyPair
getKey dd = do
    eprv <- doesFileExist (prvKeyPath dd)
    epub <- doesFileExist (pubKeyPath dd)

    if eprv && epub
            then ldKey
            else mkKey

    where
        mkKey :: IO RSAKeyPair
        mkKey = do
            putStrLn $ "Generating RSA keys..."
            keys <- generateRSAKey' 4096 65537
            writePKCS8PrivateKey keys Nothing >>= writeFile (prvKeyPath dd) 
            writePublicKey keys >>= writeFile (pubKeyPath dd)
            return keys

        ldKey :: IO RSAKeyPair
        ldKey = do
            putStrLn $ "Loading RSA keys..."
            kfl <- readFile (prvKeyPath dd)
            prv <- readPrivateKey kfl PwNone

            case (toKeyPair prv) of
                (Just k) -> return k
                Nothing -> error $ "Bad private key file! Please remove it or fix it (" ++ (prvKeyPath dd) ++ ")"
    
-- | Make sure everything is setup in the user's
-- data directory. If it's not setup, do so.
setup :: IO FilePath
setup = do
    dd <- getDataDir
    return dd

main :: IO ()
main = withSocketsDo $ withOpenSSL $ do
    putStrLn "Starting Scurry..."

    args <- getArgs
    dd   <- setup
    key  <- getKey dd

    writePKCS8PrivateKey key Nothing >>= putStrLn
    writePublicKey key >>= putStrLn
