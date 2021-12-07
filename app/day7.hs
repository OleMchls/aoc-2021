module Day7 where

import Data.List

type Pos = Int
type Fuel = Int
type Input = [Pos]

prepare :: String -> Input
prepare x = map read (words [if c == ',' then ' ' else c | c <- x])

solve :: Input -> Int
solve x = sum (map (\pos -> fuleSpend destination pos) x)
    where destination = median x

fuleSpend :: Pos -> Pos -> Fuel
fuleSpend a b = abs (b - a) * consumption
    where consumption = 1

median :: Ord a => [a] -> a
median xs = (sort xs) !! (length xs `div` 2)