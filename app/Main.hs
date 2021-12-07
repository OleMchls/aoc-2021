module Main where
import Day7 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day7.input.txt"
        putStr (show . solve . prepare $ x)
        