module Day8 where

-- unique signal pattern
type USP = [Char]
-- digital output value
type DOV = [Char]

type Input = [([USP], [DOV])]

prepare :: String -> Input
prepare = map (parse . split) . lines 
    where split = splitAt 59
          parse (uspString, _:dovString) = (words uspString, words dovString)

solve :: Input -> Int
solve input = sum (map lengthCount input)
    where lengthCount (_, dovs) = length [1 | dov <- dovs, length dov `elem` [2, 3, 4, 7]]
