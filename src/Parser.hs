module Parser
    ( parseProgram
    ) where

import Control.Applicative
import Lexer

parseProgram :: [Token] -> Bool
parseProgram xs = case findBlock xs of
                      [Token EOF _ _ _] -> True
                      ys -> error $ show $ value (head ys)

filterOptions :: [[Token] -> [Token]] -> [Token] -> [Token]
filterOptions fs ts = case filter (\ t -> kind (head t) /= Error) $ fs <*> [ts] of
                          []   -> Token Error "Error: Expected statement, received " 0 0 : ts
                          [rs] -> rs

getOpenBrace :: [Token] -> [Token]
getOpenBrace (Token OpenBrace _ a b : xs) = xs
getOpenBrace xs = Token Error "Error: Expected open brace, received " 0 0 : xs

getCloseBrace :: [Token] -> [Token]
getCloseBrace all@(Token Error _ _ _ : xs) = all
getCloseBrace (Token CloseBrace _ a b : xs) = xs
getCloseBrace xs = Token Error "Error: Expected close brace, received " 0 0 : xs

getOpenParen :: [Token] -> [Token]
getOpenParen (Token OpenParen _ a b : xs) = xs
getOpenParen xs = Token Error "Error: Expected open paren, received " 0 0 : xs

getCloseParen :: [Token] -> [Token]
getCloseParen all@(Token Error _ _ _ : xs) = all
getCloseParen (Token CloseParen _ a b : xs) = xs
getCloseParen xs = Token Error "Error: Expected close paren, received " 0 0 : xs

getAssignOp :: [Token] -> [Token]
getAssignOp all@(Token Error _ _ _ : xs) = all
getAssignOp (Token AssignOp _ a b : xs) = xs
getAssignOp xs = Token Error "Error: Expected assignment operator, received " 0 0 : xs

getId :: [Token] -> [Token]
getId all@(Token Error _ _ _ : xs) = all
getId (Token Id _ a b : xs) = xs
getId xs = Token Error "Error: Expected ID, received " 0 0 : xs

getIdType :: [Token] -> [Token]
getIdType (Token IdType _ a b : xs) = xs
getIdType xs = Token Error "Error: Expected assignment operator, received " 0 0 : xs

getStringExpr :: [Token] -> [Token]
getStringExpr (Token CharList _ a b : xs) = xs
getStringExpr xs = Token Error "Error: Expected string, received " 0 0 : xs

getBoolVal :: [Token] -> [Token]
getBoolVal (Token Boolean _ a b : xs) = xs
getBoolVal xs = Token Error "Error: Expected boolean value, received " 0 0 : xs

getBoolOp :: [Token] -> [Token]
getBoolOp (Token BoolOp _ a b : xs) = xs
getBoolOp xs = Token Error "Error: Expected boolean operator, received " 0 0 : xs

getDigit :: [Token] -> [Token]
getDigit (Token Digit _ a b : xs) = xs
getDigit xs = Token Error "Error: Expected digit, received " 0 0 : xs

getIntOp :: [Token] -> [Token]
getIntOp (Token IntOp _ a b : xs) = xs
getIntOp xs = Token Error "Error: Expected int operator, received " 0 0 : xs

findBlock :: [Token] -> [Token]
findBlock all@(Token Error _ _ _ : xs) = all
findBlock xs = (getCloseBrace . findStatementList . getOpenBrace) xs

findStatementList :: [Token] -> [Token]
findStatementList all@(Token Error _ _ _ : xs) = all
findStatementList xs = case findStatement xs of
                           all@(t : Token CloseBrace _ _ _ : ys) -> tail all
                           all@(Token Error _ _ _ : ys) -> all
                           ys -> findStatementList ys

findStatement :: [Token] -> [Token]
findStatement = filterOptions [findBlock, findIf, findWhile, findVarDecl, findAssignment, findPrint]

findPrint :: [Token] -> [Token]
findPrint (Token Print _ a b : xs) = (getCloseParen . findExpr . getOpenParen) xs
findPrint xs = Token Error "Error: Expected print statement, received " 0 0 : xs

findAssignment :: [Token] -> [Token]
findAssignment = findExpr . getAssignOp . getId

findVarDecl :: [Token] -> [Token]
findVarDecl = getId . getIdType

findWhile :: [Token] -> [Token]
findWhile (Token While _ a b : xs) = (findBlock . findBoolExpr) xs
findWhile xs = Token Error "Error: Expected while statement, received " 0 0 : xs

findIf :: [Token] -> [Token]
findIf (Token If _ a b : xs) = (findBlock . findBoolExpr) xs
findIf xs = Token Error "Error: Expected if statement, received " 0 0 : xs

findExpr :: [Token] -> [Token]
findExpr all@(Token Error _ _ _ : xs) = all
findExpr xs = filterOptions [findIntExpr, getStringExpr, findBoolExpr, getId] xs

findBoolExpr :: [Token] -> [Token]
findBoolExpr = filterOptions [getOpenParen . findExpr . getBoolOp . findExpr . getCloseParen, getBoolVal]

findIntExpr :: [Token] -> [Token]
findIntExpr = filterOptions [findExpr . getIntOp . getDigit, getDigit]

