module Interpret (interpret) where

import Ast ( Command(..), generateAst )
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import Data.Array.IO
    ( readArray, writeArray, MArray(newArray), IOArray )
import Data.Char ( ord, chr )
import System.IO ( hFlush, stdout )
import System.Directory ()
import Control.Monad ( replicateM )
import Control.Monad.Trans.Except

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

incByte :: Byte -> MutableArray -> IO ()
incByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  writeArray array byte (val + 1)

decByte :: Byte -> MutableArray -> IO ()
decByte (Byte b) array = do
  byte <- readIORef b
  val <- readArray array byte
  writeArray array byte (val - 1)

output :: Byte -> MutableArray -> IO ()
output byte array = do
  val <- readByte byte array
  if val < 0 then putStr ""
  else putStr $ chr val : "\n"

input :: Byte -> MutableArray -> IO ()
input (Byte b) array = do
  byte <- readIORef b
  insert <- getChar
  writeArray array byte (ord insert)
  putChar '\n'

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
    Increment -> do
      incByte byte array
      return $ Right ()
    Decrement -> do
      decByte byte array
      return $ Right ()
    Output -> do
      output byte array
      return $ Right ()
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

interpret = do
  arr <- newArray (1,30000) 0 :: IO MutableArray
  byte <- makeByte
  repl byte arr

convertAst :: Either String [Command] -> Byte -> MutableArray -> IO (Either String ())
convertAst (Left err) _ _ = return $ Left err
convertAst (Right tokens) byte arr = runCode tokens byte arr

repl :: Byte -> MutableArray -> IO ()
repl byte arr = do
    code <- prompt "> "
    case code of
        ":b" -> getCurrentByte byte arr
        "reset" -> interpret
        _ -> do
            let parsed = return $ generateAst code :: IO (Either String [Command])
            parsed >>= (\parse -> do
                result <- convertAst parse byte arr
                case result of
                    (Left err) -> putStrLn err
                    (Right ()) -> return ()
                )
    repl byte arr