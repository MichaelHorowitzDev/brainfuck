import Compile
import Interpret
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "What would you like to do?"
  putStrLn "1 - Compile From File"
  putStrLn "2 - Compile From Read"
  putStrLn "3 - REPL"
  putStrLn "Action: "
  action <- getChar
  putChar '\n'
  case action of
    '1' -> compileFromFile
    '2' -> do
        putStrLn "Enter Code:"
        code <- getLine
        compile code
    '3' -> interpret
    _ -> do
      putStrLn "Incorrect command"
      main

