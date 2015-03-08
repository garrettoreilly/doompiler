import Data.List
import Control.Monad
import Lexer
import Parser

main :: IO ()
main = do
    input <- getLine
    let inputList = concatMap (\(a, x) -> [(a, b, c) | (b, c) <- zip [1..] x]) $ zip [1..] $ lines input
    let tokenList = lexProgram inputList
    if kind (last tokenList) == Warning
        then do
            putStrLn $ value (last tokenList)
            print $ parseProgram $ init tokenList
        else print $ parseProgram tokenList
