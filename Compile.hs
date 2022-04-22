module Compile (compile, compileFromFile) where
import Ast
import System.IO
import System.Directory
import Data.List

fileName :: String
fileName = "brainfuck.c"

cMain :: String -> String
cMain s = "#include <stdio.h>\n\nint main() {\n" ++ s ++ "\n}"

programStart :: String
programStart = "char array[30000] = {0}; char *ptr = array;"

commands :: [Command] -> [String]
commands [] = [""]
commands (x:xs) = (++) (case x of
    IncrementPointer -> ["++ptr;"]
    DecrementPointer -> ["--ptr;"]
    Increment -> ["++*ptr;"]
    Decrement -> ["--*ptr;"]
    Output -> ["putchar(*ptr);"]
    Input -> ["*ptr = getchar();"]
    Loop expression -> "while (*ptr) {" : map ("\t"++) (filter (not . null) $ commands expression) ++ ["}"]) (commands xs)


cProgram :: [Command] -> String
cProgram xs = cMain $ unwords $ map ("\n\t"++) $ programStart : commands xs

compileFromFile :: IO ()
compileFromFile = do
  code <- readFile "brainfuck.txt"
  compile code

compile :: String -> IO ()
compile code = do
  let ast = generateAst code
  case ast of
      (Left err) -> putStrLn err
      (Right tokens) -> writeFile fileName (cProgram tokens)