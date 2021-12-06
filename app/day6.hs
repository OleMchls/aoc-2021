module Day6 where

type Input = [Int]

type Generation = [Int]

prepare :: String -> Input
prepare x = map read (words [if c == ',' then ' ' else c | c <- x])

solve :: Input -> Int
solve fish = length . last $ generations
    where generations = scanl makeNextGen fish [1..80]
          makeNextGen currentGen _ = nextGen currentGen

nextGen :: Generation -> Generation
nextGen lastGen = ([if x == 0 then 6 else x - 1 | x <- lastGen]) ++ determineBabyFish lastGen

determineBabyFish :: Generation -> Generation
determineBabyFish gen = [ 8 | x <- gen, x == 0]

-- >>> [ if x-1 == 0 then 6 else x-1 | x <- [3,4,3,1,2] ]
-- [2,3,2,6,1]

-- cabal update
-- cabal install ghci-dap haskell-debug-adapter
-- haskell-debug-adapter --version
