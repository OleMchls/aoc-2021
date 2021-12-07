module Day7 where

import Data.List

type Pos = Int
type Fuel = Int
type Input = [Pos]

prepare :: String -> Input
prepare x = map read (words [if c == ',' then ' ' else c | c <- x])

solve :: Input -> Int
solve x = (sum . map (fuleSpend destination)) x
    where destination = (floor . average) x

fuleSpend :: Pos -> Pos -> Fuel
fuleSpend a b = sum [1 .. abs (a - b)]

median :: Ord a => [a] -> a
median xs = (sort xs) !! (length xs `div` 2)

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs