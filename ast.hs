module Ast (Command (..), generateAst) where

import Lexer
import Data.Monoid

data Command = 
  Loop [Command]
  | IncrementPointer
  | DecrementPointer
  | Increment
  | Decrement
  | Output
  | Input deriving (Show, Eq)

filterChar :: String -> String
filterChar = filter (\x -> x `elem` "<>+-[].,")

bracketCount :: Char -> Int
bracketCount '[' = 1
bracketCount ']' = -1
bracketCount _ = 0

--countBrackets :: String -> Bool
--countBrackets s = filter (\x -> x `elem` "[]")
countBrackets :: String -> (String, Sum Int)
countBrackets [] = ("", Sum 0)
countBrackets (x:xs)
  | x == '[' = (xs, Sum 1)
  | x == ']' = (xs, Sum (-1))
  | otherwise = (xs, Sum 0)
countBrackets ('[':xs) = (xs, Sum 1)
countBrackets (']':xs) = (xs, Sum (-1))
countBrackets xs = (xs, Sum 0)

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y, log <> newLog)

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

generateAst :: String -> [Command]
generateAst s = fst $ getExpression $ filterChar s
