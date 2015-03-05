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

lexProgram :: String -> [Token]
lexProgram "" = []
lexProgram (x:xs)
    | x `elem` "(){}+$"   = singleCharTokens x : lexProgram xs
    | [x, head xs] == "!="   = Token BoolOp "!=" 0 0 : lexProgram xs
    | [x, head xs] == "=="   = Token BoolOp "==" 0 0 : lexProgram xs
    | x == '='            = Token AssignOp "=" 0 0 : lexProgram xs
    | x == '\"'           = stringTokens xs []
    | x `elem` ['0'..'9'] = Token Digit [x] 0 0 : lexProgram xs
    | x `elem` ['a'..'z'] = charTokens (x:xs)
    | x == ' '            = lexProgram xs
    | x == '\n'           = lexProgram xs
    | otherwise           = error "Uh...what?"

singleCharTokens :: Char -> Token
singleCharTokens '(' = Token OpenParen "(" 0 0
singleCharTokens ')' = Token CloseParen ")" 0 0
singleCharTokens '{' = Token OpenBrace "{" 0 0
singleCharTokens '}' = Token CloseBrace "}" 0 0
singleCharTokens '+' = Token IntOp "+" 0 0
singleCharTokens '$' = Token EOF "$" 0 0

stringTokens :: String -> String -> [Token]
stringTokens (x:xs) list
    | x == '\"'           = Token CharList (reverse ('\"' : list ++ "\"")) 0 0 : lexProgram xs
    | x == ' '            = stringTokens xs (x : list)
    | x `elem` ['a'..'z'] = stringTokens xs (x : list)
    | otherwise           = error "Invalid string character"

charTokens :: String -> [Token]
charTokens ('b':'o':'o':'l':'e':'a':'n':xs) = Token IdType "boolean" 0 0 : lexProgram xs
charTokens ('s':'t':'r':'i':'n':'g':xs)     = Token IdType "string" 0 0 : lexProgram xs
charTokens ('i':'n':'t':xs)                 = Token IdType "int" 0 0 : lexProgram xs
charTokens ('i':'f':xs)                     = Token If "if" 0 0 : lexProgram xs
charTokens ('w':'h':'i':'l':'e':xs)         = Token While "while" 0 0 : lexProgram xs
charTokens ('p':'r':'i':'n':'t':xs)         = Token Print "print" 0 0 : lexProgram xs
charTokens (x:xs)                           = Token Id [x] 0 0 : lexProgram xs
