import Compile
import Interpret hiding (main)
import Repl
import System.IO
import System.Process

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "What would you like to do?"
    putStrLn "1 - Interpret from File"
    putStrLn "2 - Compile From File"
    putStrLn "3 - Compile From Read"
    putStrLn "4 - REPL"
    putStrLn "Action: "
    action <- getLine
    case action of
        "1" -> interpretFromFile
        "2" -> compileFromFile
        "3" -> do
            putStrLn "Enter Code:"
            code <- getLine
            compile code
        "4" -> system "clear" >> repl
        _ -> putStrLn "Incorrect command" >> main
    main