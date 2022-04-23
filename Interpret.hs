module Interpret (interpret) where

import Ast ( Command(..), generateAst )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import Data.Array.IO
import Data.Char ( ord, chr )
import System.IO
import System.Directory ()
import Control.Monad ( replicateM )
import Control.Monad.Trans.Except
import Data.List
import Control.Exception

newtype Byte = Byte { x :: IORef Int }

makeByte :: IO Byte
makeByte = do
  iref <- newIORef 1
  return (Byte iref)

createByteArray :: Int -> IO [Byte]
createByteArray x = replicateM x makeByte

incPointer :: Byte -> IO (Either String ())
incPointer (Byte b) = do
  byte <- readIORef b
  if byte == 30000 then return $ Left "Over memory limit"
  else do
      modifyIORef b (+1)
      return $ Right ()

decPointer :: Byte -> IO (Either String ())
decPointer (Byte b) = do
  byte <- readIORef b
  if byte == 1 then return $ Left "Can't move pointer less than 1"
  else do
      modifyIORef b (subtract 1)
      return $ Right ()

readByte :: Byte -> MutableArray -> IO Int
readByte (Byte b) array = do
  byte <- readIORef b
  readArray array byte

incByte :: Byte -> MutableArray -> IO (Either String ())
incByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  if val == 65535 then return $ Left "Value cannot be greater than 65535"
  else do
      writeArray array byte (val + 1)
      return $ Right ()

decByte :: Byte -> MutableArray -> IO (Either String ())
decByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  if val == 0 then return $ Left "Value cannot be less than 0"
  else do
      writeArray array byte (val - 1)
      return $ Right ()

output :: Byte -> MutableArray -> IO (Either String ())
output byte array = do
  val <- readByte byte array
  if val < 0 then return $ Left "Byte is less than 0"
  else do
      putChar $ chr val
      return $ Right ()

input :: Byte -> MutableArray -> IO ()
input (Byte b) array = do
  byte <- readIORef b
  insert <- getChar
  writeArray array byte (ord insert)

getCurrentByte :: Byte -> MutableArray -> IO ()
getCurrentByte (Byte b) array = do
  byte <- readIORef b
  byteVal <- readByte (Byte b) array
  putStrLn $ "Current Byte: " ++ show byte ++ "\nByte Val: " ++ show byteVal

--saveFile :: Byte -> MutableArray -> IO ()
--saveFile (Byte b) array = do
  --get

loop :: Byte -> MutableArray -> [Command] -> IO (Either String ())
loop _ _ [] = return $ Right ()
loop byte array xs = do
  result <- runCode xs byte array
  case result of
    (Left err) -> return $ Left err
    (Right ()) -> checkByte $ readByte byte array
  where
    checkByte :: IO Int -> IO (Either String ())
    checkByte x = do
      val <- x
      if val == 0
        then return $ Right ()
      else loop byte array xs

runCode :: [Command] -> Byte -> MutableArray -> IO (Either String ())
runCode [] _ _ = return $ Right ()
runCode (x:xs) byte array = do
  result <- (case x of
    IncrementPointer -> incPointer byte
    DecrementPointer -> decPointer byte
    Increment -> incByte byte array
    Decrement -> decByte byte array
    Output -> output byte array
    Input -> do
      input byte array
      return $ Right ()
    Loop expression -> loop byte array expression)
  case result of
    (Left err) -> return $ Left err
    (Right ()) -> runCode xs byte array

type MutableArray = IOArray Int Int

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

main :: IO ()
main = interpret

interpret :: IO ()
interpret = do
  arr <- newArray (1,30000) 0 :: IO MutableArray
  byte <- makeByte
  replIntro
  repl byte arr

replIntro :: IO ()
replIntro = do
    putStrLn "Welcome to the Brainfuck REPL!"
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

repl :: Byte -> MutableArray -> IO ()
repl byte arr = do
    code <- prompt "> "
    case code of
        ":b" -> getCurrentByte byte arr >> repl byte arr
        "reset" -> putStrLn "Reset Memory" >> interpret
        "save" -> save arr >> repl byte arr
        "load" -> loadFromFile
        "help" -> replHelp >> repl byte arr
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
            repl byte arr

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
    repl byte arr

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen f s =
    case dropWhile f s of
        "" -> []
        s' -> w : wordsWhen f s''
            where (w, s'') = break f s'

-- indentString :: String -> String
-- indentString s =
--     let split = wordsWhen (== '\n') s
--     in intercalate "\n" $ map ("\t" ++) split

-- generateJSON :: [(Int, Int)] -> String
-- generateJSON arr =
--     let cellData = map (\(i, b) -> intercalate ",\n" [index i, byte b, value b]) arr
--         cells = intercalate ",\n" $ map (\x -> "{\n" ++ indentString x ++ "\n}") cellData
--     in cells
--     where
--         index i = "\"index\": " ++ show i
--         byte b = "\"byte\": " ++ show b
--         value b = "\"value\": " ++ "\"" ++ [chr b] ++ "\""

-- saveJSON :: MutableArray -> IO ()
-- saveJSON arr = do
--     associated <- getAssocs arr
--     let cellData = "[\n" ++ indentString (generateJSON associated) ++ "\n]"
--     let fileName = "brainfuck.json"
--     writeFile fileName cellData