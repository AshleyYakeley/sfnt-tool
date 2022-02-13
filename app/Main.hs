module Main where

import Data.ByteString.Lazy as BS hiding (putStrLn)
import OS2
import System.Environment
import Tables
import Prelude hiding (readFile, writeFile)

updateOS2 :: OS2Table -> IO OS2Table
updateOS2 t = do
    putStrLn $ "usWidthClass=" <> show (usWidthClass t)
    return t

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            oldBS <- readFile path
            oldTFile <- readTableFile oldBS
            newTFile <- modifyTable "OS/2" updateOS2 oldTFile
            let newBS = writeTableFile newTFile
            _ <- readTableFile newBS
            if oldBS == newBS
                then putStrLn "No change"
                else do
                    putStrLn "Updated"
                    writeFile path newBS
        _ -> putStrLn "Usage: sfnt-tool <file>"
