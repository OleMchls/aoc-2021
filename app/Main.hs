module Main where
import Day14 ( solve, prepare )

main :: IO ()
main = do
        x <- readFile "day14.input.txt"
        putStr (show . solve . prepare $ x)
        