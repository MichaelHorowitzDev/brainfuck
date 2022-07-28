module Interpret where

import Ast ( Command(..), generateAst )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import Data.Array.IO
import System.IO
import Data.Char ( ord, chr )
import Control.Monad ( replicateM )
import Control.Exception ( SomeException, try )
import System.Environment   
import Data.List

newtype Byte = Byte { x :: IORef Int }

makeByte :: IO Byte
makeByte = do
  iref <- newIORef 1
  return (Byte iref)

makeArray :: IO MutableArray
makeArray = newArray (1,30000) 0

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

loadFile :: String -> IO (Either SomeException String)
loadFile fileName = try $ readFile fileName :: IO (Either SomeException String)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No File"
    else do
        fileData <- loadFile $ head args
        case fileData of
            Left err -> putStrLn $ "Error: " ++ show err
            Right val -> interpret val

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine

interpretFromFile :: IO ()
interpretFromFile = do
    fileName <- prompt "File Name: "
    fileData <- loadFile fileName
    case fileData of
        Left err -> putStrLn $ "Error: " ++ show err
        Right val -> interpret val

interpret :: String -> IO ()
interpret s = case generateAst s of
    (Left err) -> putStrLn err
    (Right ast) -> do
        arr <- makeArray
        byte <- makeByte
        result <- runCode ast byte arr
        case result of
            (Left err) -> putStrLn err
            _ -> return ()

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