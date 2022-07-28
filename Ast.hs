module Ast (Command (..), generateAst) where

import Data.Monoid
import Data.List (sort)
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

joinFirst :: (Monoid m) => m -> (m,a) -> (m,a)
joinFirst a (x,y) = (a <> x, y)

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
    let (tokens, remaining) = parseExpression s
    in if remaining /= "" then Left "Mismatched Brackets"
        else let optimized = astOptimizer tokens in Right optimized