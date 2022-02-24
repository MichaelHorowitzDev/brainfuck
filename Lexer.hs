module Lexer (Token, tokenize, Tokens) where

data Token = 
  RightAngleBracket 
  | LeftAngleBracket
  | Plus
  | Minus
  | Dot
  | Comma
  | OpenBracket
  | CloseBracket deriving (Show)

getToken :: Char -> Maybe Token
getToken c = case c of
  '>' -> Just RightAngleBracket
  '<' -> Just LeftAngleBracket
  '+' -> Just Plus
  '-' -> Just Minus
  '.' -> Just Dot
  ',' -> Just Comma
  '[' -> Just OpenBracket
  ']' -> Just CloseBracket
  _ -> Nothing
type Tokens = [Token]
tokenize :: String -> Tokens
tokenize xs = foldr (\x acc -> case getToken x of Nothing -> acc; Just y -> y:acc) [] xs
