module Day6 where

import qualified Data.Map as M

type Input = Generation

type Age = Int
type Count = Int

type Generation = M.Map Age Count

prepare :: String -> Input
prepare x = foldl insertIntoGen gen0 $ map read (words [if c == ',' then ' ' else c | c <- x])
    where gen0 = M.empty
          insertIntoGen gen i = M.insertWith (+) i 1 gen

solve :: Input -> Int
solve fish = (sum . M.elems) $ generation 256
    where generation x = foldl nextGen fish [1..x]

nextGen :: Generation -> Int -> Generation
nextGen currentGen _ = M.unionWith (+) ageing born
    where ageing = M.fromList [(x - 1, currentGenCount x currentGen) | x <- [1..8]]
          born = M.fromList [(6, currentGenCount 0 currentGen), (8, currentGenCount 0 currentGen)]
          currentGenCount = M.findWithDefault 0
