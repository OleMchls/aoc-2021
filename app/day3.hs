module Day3 where

import Data.List

data Bit = Zero | One deriving (Eq, Ord, Show)
data Freq = Freq { ones, zeroes :: Int } deriving Show

instance Semigroup Freq where
  Freq o z <> Freq p x = Freq (o + p) (z + x)
instance Monoid Freq where
  mempty = Freq 0 0

toFreq :: Bit -> Freq
toFreq Zero = Freq 0 1
toFreq One = Freq 1 0

type Input = [[Bit]]

prepare :: String -> Input
prepare = map (map toBit) . lines
  where toBit '0' = Zero
        toBit '1' = One

solve :: Input -> Int
solve input = do
    let colums = transpose input
    (calculateGammaRate colums) * (calculateEpsilonRate colums)

bitsToDec :: [Bit] -> Int
bitsToDec = snd . foldr (\c (i, acc) -> (succ i, acc + (toDec c) * 2 ^ i )) (0, 0)
  where toDec One = 1
        toDec Zero = 0

calculateGammaRate :: [[Bit]] -> Int
calculateGammaRate = bitsToDec . map findMostCommon

calculateEpsilonRate :: [[Bit]] -> Int
calculateEpsilonRate = bitsToDec . map findLeastCommon

findMostCommon :: [Bit] -> Bit
findMostCommon = toBit . foldMap toFreq
  where toBit freq = if ones freq > zeroes freq then One else Zero 

findLeastCommon :: [Bit] -> Bit
findLeastCommon = toBit . foldMap toFreq
  where toBit freq = if ones freq < zeroes freq then One else Zero 
