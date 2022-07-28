module Repl where

import Interpret hiding (loadFile)
import System.IO
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import Data.Array.IO
import Data.Char ( ord, chr )
import Ast ( Command(..), generateAst )
import Data.List ( find, intercalate )
import Control.Exception ( SomeException, try )

repl :: IO ()
repl = do
    arr <- makeArray
    byte <- makeByte
    replIntro
    runRepl byte arr

runRepl :: Byte -> MutableArray -> IO ()
runRepl byte arr = do
    code <- prompt "> "
    case code of
        ":b" -> getCurrentByte byte arr >> runRepl byte arr
        "reset" -> putStrLn "Reset Memory" >> repl
        "save" -> save arr >> runRepl byte arr
        "load" -> loadFromFile
        "help" -> replHelp >> runRepl byte arr
        "q" -> return ()
        _ -> do
            let parsed = generateAst code
            case parsed of
                (Left err) -> putStrLn err
                (Right tokens) -> do
                    result <- runCode tokens byte arr
                    case result of
                        (Left err) -> putStrLn err
                        (Right ()) -> return ()
                    case find (\x -> x `elem` [Output, Input]) tokens of
                        Nothing -> putStr ""
                        Just _ -> putChar '\n'
            runRepl byte arr

save :: MutableArray -> IO ()
save arr = do
    hSetBuffering stdout NoBuffering
    putStrLn "What format would you like to save in?"
    putStrLn "1 - CSV"
    -- putStrLn "2 - JSON"
    action <- getLine
    case action of
        "1" -> saveCSV arr
        -- '2' -> saveJSON arr
        _ -> save arr

saveCSV :: MutableArray -> IO ()
saveCSV arr = do
    associated <- getAssocs arr
    let heading = "Index,Byte,Value"
    let csvData = heading : map (\(index, byte) -> show index ++ "," ++ show byte ++ "," ++ [chr byte]) associated
    let fileName = "brainfuck.csv"
    writeFile fileName $ intercalate "\n" csvData

loadFromFile :: IO ()
loadFromFile = do
    hSetBuffering stdout NoBuffering
    putStrLn "What format would you like to load from?"
    putStrLn "1 - CSV"
    -- putStrLn "2 -> JSON"
    action <- getLine
    case action of
        "1" -> loadCSV
        -- '2' -> loadJSON
        _ -> loadFromFile

loadFile :: IO String
loadFile = do
    fileName <- prompt "File Name: "
    result <- try $ readFile fileName :: IO (Either SomeException String)
    case result of
        Left ex -> do
            putStrLn $ "Error: " ++ show ex
            loadFile
        Right val -> return val

loadCSV :: IO ()
loadCSV = do
    contents <- loadFile
    let codeLines = lines contents
    let heading = head codeLines
    let dataLines = tail codeLines
    let dataPairs = map (\line -> case wordsWhen (==',') line of
            [_, byte] -> read byte
            [_, byte, _] -> read byte
            _ -> 0) dataLines :: [Int]
    arr <- newListArray (1, length dataPairs) dataPairs :: IO MutableArray
    byte <- makeByte
    runRepl byte arr



replIntro :: IO ()
replIntro = do
    putStrLn "Welcome to the Brainfuck runRepl!"
    putStrLn "Type 'help' for a list of commands."

replHelp :: IO ()
replHelp = do
    putStrLn "+ - Increment the value at the current pointer"
    putStrLn "- - Decrement the value at the current pointer"
    putStrLn "> - Move the pointer to the right"
    putStrLn "< - Move the pointer to the left"
    putStrLn ". - Output the value at the current pointer"
    putStrLn ", - Input a value into the current pointer"
    putStrLn "[ - Loop until the value at the current pointer is 0"
    putStrLn "] - Loop until the value at the current pointer is not 0"
    putStrLn "q - Quit"
    putStrLn ":b - Get the current byte"
    putStrLn "reset - Reset memory"
    putStrLn "save - Save memory to a file"
    putStrLn "load - Load a file into memory"
    putStrLn "help - Display this help message"