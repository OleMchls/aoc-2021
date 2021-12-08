module Main where
import Day8 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day8.input.txt"
        putStr (show . solve . prepare $ x)
        