module Main where

import System.Environment
import System.IO
import Lib


strToInt :: String -> String -> [Int]
strToInt []   []     = []
strToInt temp []     = [read temp]
strToInt []   (x:xs) = if x == ' ' then
                           strToInt [] xs
                       else
                           strToInt [x] xs
strToInt temp (x:xs) = if x == ' ' then
                           read temp : strToInt [] xs
                       else
                           strToInt (temp ++ [x]) xs

readToList :: Handle -> IO [[Int]]
readToList handle = do
    contents <- hGetContents handle
    return $ strToInt [] <$> lines contents

writeToText :: Handle -> String -> IO String
writeToText handle formula = do
    let declare =
            [ "(declare-fun " ++ show (Var x y) ++ " () Int)"
            | x <- [1 .. 9]
            , y <- [1 .. 9]
            ]
    let check =
            "(check-sat)\n(get-value ("
                ++ concat [show (Var x y) ++ " " | x <- [1 .. 9], y <- [1 .. 9] ]
                ++ "))"

    mapM_ (hPutStrLn handle) declare
    hPutStrLn handle $ "(assert " ++ formula ++ ")"
    hPutStr handle check
    return ""

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    readHandle <- openFile file ReadMode
    list       <- readToList readHandle
    let formula = show $ cageFormula list
    writeHandle <- openFile "output.txt" WriteMode
    writeToText writeHandle formula
    putStrLn "creating \"output.txt\" successful"
    hClose readHandle
    hClose writeHandle
