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

generateAst :: String -> Either String Ast
generateAst s =
    let (tokens, remaining) = parseExpression s
    in if remaining /= "" then Left "Mismatched Brackets"
        else Right tokens