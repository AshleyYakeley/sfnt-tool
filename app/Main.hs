module Main (main) where

import Data.ByteString.Lazy as BS hiding (putStrLn)
import OS2
import System.Environment
import Tables
import Prelude hiding (readFile, writeFile)

data Instruction = GetInstruction String | SetInstruction String Int

getProperty :: String -> Maybe (OS2Table -> Int)
getProperty "weightClass" = Just $ fromIntegral . usWeightClass
getProperty "widthClass" = Just $ fromIntegral . usWidthClass
getProperty _ = Nothing

setProperty :: String -> Maybe (Int -> OS2Table -> OS2Table)
setProperty "weightClass" = Just $ \i t -> t{usWeightClass = fromIntegral i}
setProperty "widthClass" = Just $ \i t -> t{usWidthClass = fromIntegral i}
setProperty _ = Nothing

os2Instruction :: Instruction -> OS2Table -> IO OS2Table
os2Instruction (GetInstruction name) t | Just getter <- getProperty name = do
    putStrLn $ name <> "=" <> show (getter t)
    return t
os2Instruction (GetInstruction name) _ = fail $ "Unknown property: " ++ name
os2Instruction (SetInstruction name value) t | Just setter <- setProperty name = do
    return $ setter value t
os2Instruction (SetInstruction name _) _ = fail $ "Unknown property: " ++ name

os2Instructions :: [Instruction] -> OS2Table -> IO OS2Table
os2Instructions [] t = return t
os2Instructions (i : is) t = do
    t' <- os2Instruction i t
    os2Instructions is t'

splitString :: String -> Maybe (String, String)
splitString [] = Nothing
splitString ('=' : xs) = Just ("", xs)
splitString (x : xs) = do
    (a, b) <- splitString xs
    return (x : a, b)

parseInstruction :: String -> Instruction
parseInstruction s | Just (a, b) <- splitString s = SetInstruction a (read b)
parseInstruction s = GetInstruction s

parseInstructions :: MonadFail m => [String] -> m ([Instruction], String)
parseInstructions [] = fail "No path specified"
parseInstructions [path] = return ([], path)
parseInstructions (x : xs) = do
    (a, b) <- parseInstructions xs
    return (parseInstruction x : a, b)

main :: IO ()
main = do
    args <- getArgs
    (instructions, path) <- parseInstructions args
    oldBS <- readFile path
    oldTFile <- readTableFile oldBS
    newTFile <- modifyTable "OS/2" (os2Instructions instructions) oldTFile
    let newBS = writeTableFile newTFile
    _ <- readTableFile newBS
    if oldBS == newBS
        then putStrLn "No change"
        else do
            putStrLn "Updated"
            writeFile path newBS
