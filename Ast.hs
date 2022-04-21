module Ast (Command (..), generateAst) where

import Lexer
import Data.Monoid
import Data.List (sort)

data Command =
  Loop [Command]
  | IncrementPointer
  | DecrementPointer
  | Increment
  | Decrement
  | Output
  | Input deriving (Show, Eq)

countBrackets :: String -> Bool
countBrackets xs = 
    even (length sorted) 
    && filtered == sorted
    where
        filtered = filter (`elem` "[]") xs
        sorted = sort filtered

joinFirst :: (Monoid m) => m -> (m,a) -> (m,a)
joinFirst a (x,y) = (a <> x, y)

getExpression :: String -> ([Command], String)
getExpression [] = ([], "")
getExpression (x:xs) = case x of
  '[' -> [Loop tokens] `joinFirst` getExpression string
  ']' -> ([], xs)
  '+' -> [Increment] `joinFirst` getExpression xs
  '-' -> [Decrement] `joinFirst` getExpression xs
  '>' -> [IncrementPointer] `joinFirst` getExpression xs
  '<' -> [DecrementPointer] `joinFirst` getExpression xs
  '.' -> [Output] `joinFirst` getExpression xs
  ',' -> [Input] `joinFirst` getExpression xs
  where (tokens, string) = getExpression xs

filterChar :: String -> String
filterChar = filter (`elem` "[]<>+-.,")

generateAst :: String -> Either String [Command]
generateAst s
    | brackets = Right $ fst $ getExpression $ filterChar s
    | otherwise = Left "Brackets mismatched"
    where brackets = countBrackets s