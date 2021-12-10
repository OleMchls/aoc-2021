module Main where
import Day10 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day10.input.txt"
        putStr (show . solve . prepare $ x)
        