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

incByte :: Byte -> MutableArray -> IO ()
incByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  if val == 65535 then writeArray array byte 0
  else writeArray array byte (val + 1)

decByte :: Byte -> MutableArray -> IO ()
decByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  if val == 0 then writeArray array byte 65535
  else writeArray array byte (val - 1)

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

loop :: Byte -> MutableArray -> [Command] -> IO (Either String ())
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
    Increment -> do
        incByte byte array
        return $ Right ()
    Decrement -> do
        decByte byte array
        return $ Right ()
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