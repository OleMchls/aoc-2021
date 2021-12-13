module Main where
import Day11 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day11.input.txt"
        putStr (show . solve . prepare $ x)
        