module Main where
import Day3 (solve, prepare)

main :: IO ()
main = do
        x <- readFile "day3.input.txt"
        putStr (show . solve . prepare $ x)
        