module Day9 where
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Maybe (isNothing)

type CaveMap = M.Map Coord Height

type CoordX = Int
type CoordY = Int
type Coord = (CoordX, CoordY)
type Field = (Coord, Height)

type Height = Int

type Input = CaveMap

prepare :: String -> Input
prepare = toCoords . map (map digitToInt) . lines

toCoords :: [[Height]] -> Input
toCoords lines = M.fromList . concat $ [[((y, x), v) | (x, v) <- zip [0 ..] row] | (y, row) <- zip [0 ..] lines]

solve :: Input -> Int
solve = sum . map calculateRiskLevel . findLowPoints
    where calculateRiskLevel (_, h) = h + 1

findLowPoints :: CaveMap -> [Field]
findLowPoints cave = M.toList . M.filterWithKey lowestNeighbour $ cave
    where lowestNeighbour coords height = height < minimum (map snd (neighbours coords))
          neighbours = adjacentLocations cave

adjacentLocations :: CaveMap -> Coord -> [Field]
adjacentLocations cave (x, y) = foldl pickCoordsFromMap [] searchCoords
    where searchCoords = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
          pickCoordsFromMap fields coords = if isNothing (cave M.!? coords) then fields else (coords, cave M.! coords):fields