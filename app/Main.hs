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
            inputBS <- readFile path
            tfile <- readTableFile inputBS
            putStrLn $ show (tfScalerType tfile, fmap (\t -> (tTag t, BS.length $ tData t)) $ tfTables tfile)
            let outputBS = writeTableFile tfile
            tfile' <- readTableFile outputBS
            if tfile == tfile'
                then return ()
                else fail "Table file output is not the same as input"
        _ -> putStrLn "Usage: sfnt-tool <file>"
