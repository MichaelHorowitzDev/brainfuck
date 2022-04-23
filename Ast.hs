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

type Ast = [Command]

joinFirst :: (Monoid m) => m -> (m,a) -> (m,a)
joinFirst a (x,y) = (a <> x, y)

getExpression :: String -> (Ast, String)
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

-- optimize ast to not create errors such as infinite loops
astOptimizer :: Ast -> Ast
astOptimizer [] = []
astOptimizer (x:xs) =
    case x of
        (Loop tokens) ->
            let children = astOptimizer tokens
            in if null children 
                then astOptimizer xs
                else Loop children : astOptimizer xs
        _ -> x : astOptimizer xs


generateAst :: String -> Either String Ast
generateAst s =
    let (tokens, remaining) = getExpression s
    in if remaining /= "" then Left "Mismatched Brackets"
        else let optimized = astOptimizer tokens in Right optimized