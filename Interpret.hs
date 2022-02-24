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
  else modifyIORef b (+1)
  -- where
  --   do
  --     byte <- readIORef b
  --     byte

decPointer :: Byte -> Either String (IO ())
decPointer (Byte b) = do
  byte <- Right $ readIORef b
  if byte == 1 then Left "Can't move pointer less than 1"
  else Right $ modifyIORef b (subtract 1)

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
  else putChar $ chr val

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

loop :: Byte -> MutableArray -> [Command] -> Either String (IO ())
loop _ _ [] = Right $ return ()
loop byte array xs = Right $ return ()
  -- runCode xs (byte) array
  -- case readByte byte array of
  --   (Right val) ->
  -- val <- readByte byte array
  -- case val of
  --   0 -> Left ""
  --   _ -> Right $ return ()
  -- Left ""
  -- if val == 0
  --   then Right $ return ()
  -- else loop byte array xs
  -- where 
  --     checkByte :: IO Int -> Either String (IO ())
  --     checkByte x = do
  --       val <- x
  --       if val == 0
  --         then Right $ return ()
  --       else loop byte array xs
          


runCode :: [Command] -> Byte -> MutableArray -> Either String (IO ())
runCode [] _ _ = Right $ return ()
runCode (x:xs) byte array = do
  (case x of
    IncrementPointer -> incPointer byte
    DecrementPointer -> decPointer byte
    Increment -> Right $ incByte byte array
    Decrement -> Right $ decByte byte array
    Output -> Right $ output byte array
    Input -> Right $ input byte array
    Loop expression -> loop byte array expression)

  runCode xs byte array

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

repl :: Byte -> MutableArray -> IO ()
repl byte arr = do
  code <- prompt "> "
  case code of
    ":b" -> getCurrentByte byte arr
    "reset" -> interpret
    _ -> do
      let parsed = generateAst code
      let result = runCode parsed byte arr
      case result of
          (Left err) -> do
              putStrLn err
              repl byte arr
          (Right correct) -> correct
      if Output `elem` parsed
        then putStrLn ""
      else
        putStr ""
  repl byte arr

showError :: Either String (IO ()) -> IO ()
showError (Left err) = putStrLn err
showError (Right correct) = correct