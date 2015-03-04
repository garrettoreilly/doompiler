import Data.List
import Data.Char

getProgram :: IO [String]
getProgram = do
    ln <- getLine
    case map toLower ln of
        "eof" -> return []
        _ -> fmap (ln:) getProgram

main :: IO ()
main = do
    putStrLn "Enter program:"
    program <- getProgram
    putStrLn "This is your program:"
    putStrLn $ intercalate "\n" program
