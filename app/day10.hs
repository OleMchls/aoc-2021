module Day10 where

import Data.List

prepare :: String -> [String]
prepare = lines

solve :: [String] -> Int
solve = median . map scoreMissing . filter (not . null) . map (findClose [])

scoreMissing :: [Char] -> Int
scoreMissing = foldl (\acc c -> acc * 5 + score c) 0

score :: Char -> Int
score ')' = 1
score ']' = 2
score '}' = 3
score '>' = 4

findClose :: [Char] -> [Char] -> [Char]
findClose x [] = x
findClose close ('(':xs) = findClose (')':close) xs
findClose close ('[':xs) = findClose (']':close) xs
findClose close ('{':xs) = findClose ('}':close) xs
findClose close ('<':xs) = findClose ('>':close) xs
findClose (close:cs) (x:xs)
    | x == close = findClose cs xs
findClose [] (x:xs) = findClose [] xs
findClose x y = [] -- corrupted

median :: Ord a => [a] -> a
median xs = (sort xs) !! (length xs `div` 2)