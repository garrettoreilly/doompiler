module Parser
    ( parseProgram
    ) where

import Control.Applicative
import Lexer

parseProgram :: [Token] -> Bool
parseProgram xs = case findBlock xs of
                      [Token EOF _ _ _] -> True
                      [Token Error _ _ _, Token EOF _ _ _] -> True
                      (y:ys) -> error $ value y ++ show (kind (head ys)) ++ "\nLine " ++ show (line (head ys)) ++ ", position " ++ show (position (head ys))

getOpenBrace :: [Token] -> [Token]
getOpenBrace all@(Token Error _ _ _ : xs) = all
getOpenBrace (Token OpenBrace _ a b : xs) = xs
getOpenBrace xs = Token Error "Error! Expected open brace, received " 0 0 : xs

getCloseBrace :: [Token] -> [Token]
getCloseBrace all@(Token Error _ _ _ : xs) = all
getCloseBrace (Token CloseBrace _ a b : xs) = xs
getCloseBrace xs = Token Error "Error! Expected close brace, received " 0 0 : xs

getOpenParen :: [Token] -> [Token]
getOpenParen all@(Token Error _ _ _ : xs) = all
getOpenParen (Token OpenParen _ a b : xs) = xs
getOpenParen xs = Token Error "Error! Expected open paren, received " 0 0 : xs

getCloseParen :: [Token] -> [Token]
getCloseParen all@(Token Error _ _ _ : xs) = all
getCloseParen (Token CloseParen _ a b : xs) = xs
getCloseParen xs = Token Error "Error! Expected close paren, received " 0 0 : xs

getAssignOp :: [Token] -> [Token]
getAssignOp all@(Token Error _ _ _ : xs) = all
getAssignOp (Token AssignOp _ a b : xs) = xs
getAssignOp xs = Token Error "Error! Expected assignment operator, received " 0 0 : xs

getId :: [Token] -> [Token]
getId all@(Token Error _ _ _ : xs) = all
getId (Token Id _ a b : xs) = xs
getId xs = Token Error "Error! Expected ID, received " 0 0 : xs

getIdType :: [Token] -> [Token]
getIdType all@(Token Error _ _ _ : xs) = all
getIdType (Token IdType _ a b : xs) = xs
getIdType xs = Token Error "Error! Expected assignment operator, received " 0 0 : xs

getStringExpr :: [Token] -> [Token]
getStringExpr all@(Token Error _ _ _ : xs) = all
getStringExpr (Token CharList _ a b : xs) = xs
getStringExpr xs = Token Error "Error! Expected string, received " 0 0 : xs

getBoolVal :: [Token] -> [Token]
getBoolVal all@(Token Error _ _ _ : xs) = all
getBoolVal (Token Boolean _ a b : xs) = xs
getBoolVal xs = Token Error "Error! Expected boolean value, received " 0 0 : xs

getBoolOp :: [Token] -> [Token]
getBoolOp all@(Token Error _ _ _ : xs) = all
getBoolOp (Token BoolOp _ a b : xs) = xs
getBoolOp xs = Token Error "Error! Expected boolean operator, received " 0 0 : xs

getDigit :: [Token] -> [Token]
getDigit all@(Token Error _ _ _ : xs) = all
getDigit (Token Digit _ a b : xs) = xs
getDigit xs = Token Error "Error! Expected digit, received " 0 0 : xs

getIntOp :: [Token] -> [Token]
getIntOp all@(Token Error _ _ _ : xs) = all
getIntOp (Token IntOp _ a b : xs) = xs
getIntOp xs = Token Error "Error! Expected int operator, received " 0 0 : xs

findBlock :: [Token] -> [Token]
findBlock all@(Token Error _ _ _ : xs) = all
findBlock xs = (getCloseBrace . findStatementList . getOpenBrace) xs

findStatementList :: [Token] -> [Token]
findStatementList all@(Token Error _ _ _ : xs) = all
findStatementList all@(Token CloseBrace _ _ _ : xs) = all
findStatementList xs = case findStatement xs of
                           all@(Token Error _ _ _ : ys) -> all
                           ys -> findStatementList ys

findStatement :: [Token] -> [Token]
findStatement xs = compareList xs $ [findBlock, findIf, findWhile, findVarDecl, findAssignment, findPrint] <*> [xs]

findPrint :: [Token] -> [Token]
findPrint (Token Print _ a b : xs) = (getCloseParen . findExpr . getOpenParen) xs
findPrint xs = Token Error "Error! Expected print statement, received " 0 0 : xs

findAssignment :: [Token] -> [Token]
findAssignment = findExpr . getAssignOp . getId

findVarDecl :: [Token] -> [Token]
findVarDecl = getId . getIdType

findWhile :: [Token] -> [Token]
findWhile (Token While _ a b : xs) = (findBlock . findBoolExpr) xs
findWhile xs = Token Error "Error! Expected while statement, received " 0 0 : xs

findIf :: [Token] -> [Token]
findIf (Token If _ a b : xs) = (findBlock . findBoolExpr) xs
findIf xs = Token Error "Error! Expected if statement, received " 0 0 : xs

findExpr :: [Token] -> [Token]
findExpr all@(Token Error _ _ _ : xs) = all
findExpr xs = compareList xs $ [findIntExpr, getStringExpr, findBoolExpr, getId] <*> [xs]

findBoolExpr :: [Token] -> [Token]
findBoolExpr all@(Token Error _ _ _ : xs) = all
findBoolExpr xs = compareList xs $ [getCloseParen . findExpr . getBoolOp . findExpr . getOpenParen, getBoolVal] <*> [xs]

findIntExpr :: [Token] -> [Token]
findIntExpr all@(Token Error _ _ _ : xs) = all
findIntExpr xs = compareList xs $ [findExpr . getIntOp . getDigit, getDigit] <*> [xs]

compareList :: [Token] -> [[Token]] -> [Token]
compareList ts [] = Token Error "Error! Invalid token, received " 0 0 : ts
compareList ts (x:xs)
    | kind (head x) /= Error = x
    | tail x /= ts = x
    | tail x == ts = compareList ts xs
