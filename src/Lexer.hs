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

lexProgram :: [(Int, Int, Char)] -> [Token]
lexProgram [] = []
lexProgram all@((a, b, c):xs)
    | c `elem` "(){}+$"     = singleCharTokens (head all) : lexProgram xs
    | [c, getChar1] == "!=" = Token BoolOp "!=" a b : lexProgram xs
    | [c, getChar1] == "==" = Token BoolOp "==" a b : lexProgram xs
    | c == '='              = Token AssignOp "=" a b : lexProgram xs
    | c == '\"'             = stringTokens xs []
    | c `elem` ['0'..'9']   = Token Digit [c] a b : lexProgram xs
    | c `elem` ['a'..'z']   = charTokens all
    | c == ' '              = lexProgram xs
    | c == '\n'             = lexProgram xs
    | otherwise             = error $ "Error: Meow, invalid character\nLine " ++ show a ++ ", position " ++ show b
    where getChar1 = (\(_, _, x) -> x) $ head xs

singleCharTokens :: (Int, Int, Char) -> Token
singleCharTokens (a, b, '(') = Token OpenParen "(" a b
singleCharTokens (a, b, ')') = Token CloseParen ")" a b
singleCharTokens (a, b, '{') = Token OpenBrace "{" a b
singleCharTokens (a, b, '}') = Token CloseBrace "}" a b
singleCharTokens (a, b, '+') = Token IntOp "+" a b
singleCharTokens (a, b, '$') = Token EOF "$" a b

stringTokens :: [(Int, Int, Char)] -> String -> [Token]
stringTokens ((a, b, c):xs) list
    | c == '\"'           = Token CharList (reverse ('\"' : list ++ "\"")) a b : lexProgram xs
    | c == ' '            = stringTokens xs (c : list)
    | c `elem` ['a'..'z'] = stringTokens xs (c : list)
    | otherwise           = error $ "Error: Invalid CharList token\nLine " ++ show a ++ ", position " ++ show (b - length list - 1)

charTokens :: [(Int, Int, Char)] -> [Token]
charTokens xs
    | getKeyword 7 == "boolean" = Token IdType "boolean" a b : lexProgram (drop 7 xs)
    | getKeyword 6 == "string"  = Token IdType "string" a b : lexProgram (drop 6 xs)
    | getKeyword 3 == "int"     = Token IdType "int" a b : lexProgram (drop 3 xs)
    | getKeyword 2 == "if"      = Token If "if" a b : lexProgram (drop 2  xs)
    | getKeyword 5 == "while"   = Token While "while" a b : lexProgram (drop 5 xs)
    | getKeyword 5 == "print"   = Token Print "print" a b : lexProgram (drop 5 xs)
    | otherwise                 = Token Id [(\(_, _, x) -> x) $ head xs] a b : lexProgram (tail xs)
    where getKeyword x = map (\(_, _, c) -> c) $ take x xs
          a = (\(a, _, _) -> a) $ head xs
          b = (\(_, b, _) -> b) $ head xs
