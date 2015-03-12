import Data.List
import Control.Monad
import Control.Applicative
import Lexer
import Parser

main :: IO ()
main = do
    input <- getContents
    let inputList = concatMap (\(a, x) -> [(a, b, c) | (b, c) <- zip [1..] x]) $ zip [1..] $ (++) <$> lines input <*> ["\n"]
    let tokenList = lexProgram inputList
    if kind (last tokenList) == Warning
        then do
            putStrLn $ "doompiler: " ++ value (last tokenList)
            print $ parseProgram $ init tokenList
        else print $ parseProgram tokenList
