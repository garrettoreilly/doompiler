module Lexer (lexProgram) where

import Data.List
import Data.Maybe
import Data.Functor
import Control.Applicative

data TokenType = Character
               | CharList
               | Digit
               | IntOp
               | AssignOp
               | Boolean
               | BoolOp
               | Space
               | Id
               | IdType
               | Print
               | If
               | While
               | OpenParen
               | CloseParen
               | OpenBrace
               | CloseBrace
               | EOF
               deriving (Eq, Show)

data Token = Token { kind :: TokenType, value :: String, line :: Int, position :: Int }
    deriving (Show)

lexProgram :: [(Int, Int, String)] -> [Token]
lexProgram [] = []
lexProgram all@((a, b, c):xs)
    | head c `elem` "(){}+$"     = singleCharTokens (head all) : lexProgram xs
    | [head c, getChar1] == "!=" = Token BoolOp "!=" a b : lexProgram xs
    | [head c, getChar1] == "==" = Token BoolOp "==" a b : lexProgram xs
    | head c == '='              = Token AssignOp "=" a b : lexProgram xs
    | head c == '\"'             = stringTokens xs []
    | head c `elem` ['0'..'9']   = Token Digit c a b : lexProgram xs
    | head c `elem` ['a'..'z']   = charTokens all
    | head c == ' '              = lexProgram xs
    | head c == '\n'             = lexProgram xs
    | otherwise                  = error "Error: Uh...what?"
    where getChar1 = (\(_, _, x) -> head x) $ head xs

singleCharTokens :: (Int, Int, String) -> Token
singleCharTokens (a, b, "(") = Token OpenParen "(" a b
singleCharTokens (a, b, ")") = Token CloseParen ")" a b
singleCharTokens (a, b, "{") = Token OpenBrace "{" a b
singleCharTokens (a, b, "}") = Token CloseBrace "}" a b
singleCharTokens (a, b, "+") = Token IntOp "+" a b
singleCharTokens (a, b, "$") = Token EOF "$" a b

stringTokens :: [(Int, Int, String)] -> String -> [Token]
stringTokens ((a, b, c):xs) list
    | head c == '\"'           = Token CharList (reverse ('\"' : list ++ "\"")) a b : lexProgram xs
    | head c == ' '            = stringTokens xs (c ++ list)
    | head c `elem` ['a'..'z'] = stringTokens xs (c ++ list)
    | otherwise           = error "Error: Invalid string character"

charTokens :: [(Int, Int, String)] -> [Token]
charTokens xs
    | getKeyword 7 == "boolean" = Token IdType "boolean" a b : lexProgram (tail xs)
    | getKeyword 6 == "string"  = Token IdType "string" a b : lexProgram (tail xs)
    | getKeyword 3 == "int"     = Token IdType "int" a b : lexProgram ( tail xs)
    | getKeyword 2 == "if"      = Token If "if" a b : lexProgram (tail xs)
    | getKeyword 5 == "while"   = Token While "while" a b : lexProgram (tail xs)
    | getKeyword 5 == "print"   = Token Print "print" a b : lexProgram (tail xs)
    | otherwise                 = Token Id ((\(_, _, x) -> x) $ head xs) a b : lexProgram (tail xs)
    where getKeyword x = concatMap (\(_, _, x) -> x) $ take x xs
          a = (\(a, _, _) -> a) $ head xs
          b = (\(_, b, _) -> b) $ head xs
