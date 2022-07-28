module Ast (Command (..), generateAst) where

import Data.Bifunctor (first)

data Command =
  Loop [Command]
  | IncrementPointer
  | DecrementPointer
  | Increment
  | Decrement
  | Output
  | Input deriving (Show, Eq)

type Ast = [Command]

parseChar :: Char -> Command
parseChar c = case c of
    '+' -> Increment
    '-' -> Decrement
    '>' -> IncrementPointer
    '<' -> DecrementPointer
    '.' -> Output
    ',' -> Input

parseExpression :: String -> (Ast, String)
parseExpression [] = ([], [])
parseExpression xs@('[':xs') = case expr of
    (ast, ']':ys) -> first (Loop ast :) $ parseExpression ys
    _ -> ([], xs)
    where expr = parseExpression xs'
parseExpression xs@(']':_) = ([], xs)
parseExpression (x:xs) = first (parseChar x :) $ parseExpression xs

generateError :: String -> String
generateError s@('[':_) = "no matching close bracket for open bracket\n" ++ s
generateError s@(']':_) = "close bracket with no open bracket\n" ++ s
generateError s = "unknown error\n" ++ s

generateAst :: String -> Either String Ast
generateAst s =
    if (not . null) remaining 
        then Left $ "\ESC[31m\ESC[1m" ++ "error: " ++ "\ESC[0m" ++ generateError remaining 
    else Right tokens
    where (tokens, remaining) = parseExpression $ filter (`elem` "[]+-<>,.") s