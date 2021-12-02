module Day2 where

import Text.Regex.TDFA

type Command = (Direction, Distance)
type Direction = String
type Distance = Int

type Pos = (Int, Int)

solve :: String -> Int
solve x = computeResult (foldl navigate (0, 0) ((parseCommands . splitLines) x))

splitLines :: String -> [String]
splitLines x = lines x

parseCommands :: [String] -> [Command]
parseCommands x = map parseCommand x

parseCommand :: String -> Command
parseCommand line = convertCommandPairs (words line)

convertCommandPairs :: [String] -> Command
convertCommandPairs [dir, dist] = (dir, read dist :: Int)

navigate :: Pos -> Command -> Pos
navigate (x, y) ("forward", dis) = (x + dis, y)
navigate (x, y) ("down", dis) = (x, y + dis)
navigate (x, y) ("up", dis) = (x, y - dis)

computeResult :: Pos -> Int
computeResult (x, y) = x * y