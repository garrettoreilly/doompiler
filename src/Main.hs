import Data.List
import Data.Char
import Lexer

getProgram :: IO [String]
getProgram = do
    line <- getLine
    case map toLower line of
        "eof" -> return []
        _ -> fmap (line:) getProgram

main :: IO ()
main = do
    putStrLn "Enter program:"
    input <- getProgram
    let inputList = concatMap (\(a, x) -> [(a, b, c) | (b, c) <- zip [1..] x]) $ zip [1..] $ map (++ "\n") input
    let tokenList = lexProgram inputList
    print tokenList
