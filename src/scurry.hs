module Main where

import System.Console.GetOpt
import System.Directory
import System.Environment

appName :: String
appName = "scurry"

getDataDir :: IO FilePath
getDataDir = do
    p <- getAppUserDataDirectory appName
    createDirectoryIfMissing True p
    return p
    

main :: IO ()
main = do
    args <- getArgs
    dd <- getDataDir

    putStrLn "Scurry"
    putStrLn $ "Using data dir: " ++ dd
