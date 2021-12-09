module Main where
import Day9 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day9.input.txt"
        putStr (show . solve . prepare $ x)
        