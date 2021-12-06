module Main where
import Day6 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day6.input.txt"
        putStr (show . solve . prepare $ x)
        