import Data.List
import Control.Monad
import Control.Applicative
import Lexer
import Parser

main :: IO ()
main = do
    input <- getContents
    -- | The list of characters that make up the source code is zipped with their respective 
    --   line and position in order to allow lex to identify the location of each generated token.
    let inputList = concatMap (\(a, x) -> [(a, b, c) | (b, c) <- zip [1..] x]) $ zip [1..] $ (++) <$> lines input <*> ["\n"]
    let tokenList = lexProgram inputList
    -- A warning is printed if the EOF token is missing.
    if kind (last tokenList) == Warning
        then do
            putStrLn $ "doompiler: " ++ value (last tokenList)
            print $ parseProgram $ init tokenList
        else print $ parseProgram tokenList
