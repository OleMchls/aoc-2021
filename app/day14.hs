module Day14 where

import Data.List
import qualified Data.Map as M

type Polymer = String
type Dict = M.Map String String

type Counts = M.Map Char Int

type Input = (Polymer, Dict)

prepare :: String -> Input
prepare = parseDict' . splitTemplate . lines
    where splitTemplate (t:_:d) = (t, d)
          parseDict' (p, i) = (p, parseDict i)

parseDict :: [String] -> Dict
parseDict = M.fromList . map splitString
    where splitString (a:b:r) = ([a, b], [last r] )

solve :: Input -> Int
solve (start, dict) = calculateResult . rollup . generation $ 10
    where next p _ = step dict p
          generation x = foldl next start [1..x]

step :: Dict -> Polymer -> Polymer
step _ [r] = [r]
step dict (a:b:r) = a : lookup [a, b] : step dict (b:r)
    where lookup k = head (dict M.! k)

rollup :: Polymer -> Counts
rollup = foldl inc empty
    where empty = M.fromList (zip ['A'..'Z'] (repeat 0))
          inc m k = M.adjust (+1) k m

calculateResult :: Counts -> Int
calculateResult counts = mostCommon - leastCommon
    where mostCommon = counts M.! mostCommonKey
          leastCommon = counts M.! leastCommonKey
          mostCommonKey = fst . last . sortOn snd . filter (\(_, c) -> c > 0) . M.toList $ counts
          leastCommonKey = fst . head . sortOn snd . filter (\(_, c) -> c > 0) . M.toList $ counts
