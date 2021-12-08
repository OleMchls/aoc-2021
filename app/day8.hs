module Day8 where

import qualified Data.Map as M
import Data.List

-- unique signal pattern
type USP = [Char]
-- digital output value
type DOV = [Char]

type Input = [([USP], [DOV])]

type Decoder = M.Map USP Int

prepare :: String -> Input
prepare = map (parse . split) . lines 
    where split = splitAt 59
          parse (uspString, _:dovString) = (words uspString, words dovString)

solve :: Input -> Int
solve = sum . map (\(usps, dovs) -> decodeDovs (buildDecoder (map sort usps)) (map sort dovs))

decodeDovs :: Decoder -> [DOV] -> Int
decodeDovs decoder = toDec . map lookup
    where lookup dov = decoder M.! dov
          toDec a = sum (zipWith (*) (reverse a) [10 ^ x | x <- [0 ..]])

simpleDecoder :: Decoder
simpleDecoder = M.fromList [("acedgfb", 8), ("cdfbe", 5), ("gcdfa", 2), ("fbcad", 3), ("dab", 7), ("cefabd", 9), ("cdfgeb", 6), ("eafb", 4), ("cagedb", 0), ("ab", 1)]

buildDecoder :: [USP] -> Decoder
buildDecoder xs = M.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
    where one   = head (whereLength 2 xs)
          seven = head (whereLength 3 xs)
          four  = head (whereLength 4 xs)
          eight = head (whereLength 7 xs)
          zero = eight \\ d
          two = eight \\ (b ++ f)
          three = eight \\ be
          five = six \\ e
          six = head ((whereLength 6 xs) \\ (nine:zero:[]))
          nine = eight \\ e
          a = seven \\ one
          b = four \\ (one ++ d)
          be = whereCount 1 (whereLength 5 xs)
          e = be \\ b
          d = three \\ (one ++ a ++ g)
          f = whereCount 9 xs
          g = three \\ (four ++ a)
          whereLength qLenght list = filter (\x -> length x == qLenght) list
          whereCount count val = M.keys (M.filter (==count) (M.fromListWith (+) (zip (foldl (++) "" val) (cycle [1]))))