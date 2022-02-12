module Main where

import Data.ByteString.Lazy as BS hiding (putStrLn)
import System.Environment
import Tables
import Prelude hiding (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            bs <- readFile path
            tfile <- readTableFile bs
            putStrLn $ show (tfScalerType tfile, fmap tTag $ tfTables tfile)
        _ -> putStrLn "Usage: sfnt-tool <file>"
