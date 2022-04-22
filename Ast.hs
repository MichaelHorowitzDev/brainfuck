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

joinFirst :: (Monoid m) => m -> (m,a) -> (m,a)
joinFirst a (x,y) = (a <> x, y)

getExpression :: String -> ([Command], String)
getExpression "" = ([], "")
getExpression (x:xs) = case x of
    '[' -> case remaining of
            "" -> ([], x:xs)
            (a:as) ->
                if a /= ']'
                    then ([], x:xs)
                else let (remainingTokens, string) = getExpression as
                    in (Loop tokens : remainingTokens, string)
    ']' -> ([], x:xs)
    '+' -> (Increment : tokens, remaining)
    '-' -> (Decrement : tokens, remaining)
    '>' -> (IncrementPointer : tokens, remaining)
    '<' -> (DecrementPointer : tokens, remaining)
    '.' -> (Output : tokens, remaining)
    ',' -> (Input : tokens, remaining)
    _ -> getExpression xs
    where (tokens, remaining) = getExpression xs

generateAst :: String -> Either String [Command]
generateAst s =
    let (tokens, remaining) = getExpression s
    in if remaining /= "" then Left "Mismatched Brackets"
        else Right tokens