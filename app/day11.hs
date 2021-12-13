module Day11 where

import qualified Data.Map as M
import Data.Char (digitToInt)

data Octopus = Octopus {value :: Int, flashed :: Bool} deriving (Show)
type Field = (Coord, Octopus)
type Coord = (Int, Int)

type OctopusMap = M.Map Coord Octopus

prepare :: String -> OctopusMap
prepare = M.fromList . concat . buildMap . map (map digitToInt) . lines
    where buildMap s = [[ ((x,y), Octopus { value = v, flashed = False }) | (x, v) <- zip [0..] row ] | (y, row) <- zip [0..] s]

solve :: OctopusMap -> Int
solve ops = fst . last . takeWhileInclusive (not. allFlashing) $ yieldStates
    where countFlashes = length . M.elems . M.filter (\o -> value o == 0)
          yieldStates = scanl (\(_, m) c -> (c, step m)) (0, ops) [1..]
          allFlashing (_, m) = all ((==0) . value) . M.elems $ m

step :: OctopusMap -> OctopusMap
step = resetFlashed . flashOctopusses . tickAllOctopusses

tickAllOctopusses :: OctopusMap -> OctopusMap
tickAllOctopusses x = tickOctopusses (M.keys x) x

tickOctopusses :: [Coord] -> OctopusMap -> OctopusMap
tickOctopusses cs m = foldl (flip (M.adjust tickOctopus)) m cs

tickOctopus :: Octopus -> Octopus
tickOctopus o = o { value = value o + 1 }

flashOctopusses :: OctopusMap -> OctopusMap
flashOctopusses ops
    | not (null toFlash) = flashOctopusses (flashAndUpdate (toFlash, ops))
    | otherwise = ops
    where mustFlash o = value o > 9 && not (flashed o)
          toFlash = (M.keys . M.filter mustFlash) ops

-- single run of increasing neighbours and setting octopus to flashed
flashAndUpdate :: ([Coord], OctopusMap) -> OctopusMap
flashAndUpdate ([], m) = m
flashAndUpdate ((x, y) : ns, m) = flashAndUpdate (ns, tickOctopusses neightbours (updateFlashed m))
    where updateFlashed = M.adjust (\o -> o {flashed = True}) (x, y)
          neightbours = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)] -- neightbours of n
-- flashAndUpdate = M.map neightbours
-- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
-- foldr :: (a -> b -> b) -> b -> Map k a -> b

resetFlashed :: OctopusMap -> OctopusMap
resetFlashed = M.map resetOctopus
    where resetOctopus (Octopus _ True) = Octopus { value = 0, flashed = False }
          resetOctopus o = o

-- There must be a better way to yield States until condition matches
-- https://stackoverflow.com/questions/22472536/does-haskell-have-a-takeuntil-function
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x :
  if p x
    then takeWhileInclusive p xs
    else []