module Main where
import Day2 (solve)

main :: IO ()
main = do
        x <- readFile "day2.input.txt"
        putStr (show (solve x))
        