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
    let tokenList = lexProgram $ intercalate "\n" input
    print tokenList
