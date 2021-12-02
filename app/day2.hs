module Day2 where

import Text.Regex.TDFA

type Command = (Direction, Distance)
type Direction = String
type Distance = Int

type Pos = (Int, Int, Int)

solve :: String -> Int
solve x = computeResult (foldl navigate (0, 0, 0) ((parseCommands . splitLines) x))

splitLines :: String -> [String]
splitLines x = lines x

parseCommands :: [String] -> [Command]
parseCommands x = map parseCommand x

parseCommand :: String -> Command
parseCommand line = convertCommandPairs (words line)

convertCommandPairs :: [String] -> Command
convertCommandPairs [dir, dist] = (dir, read dist :: Int)

navigate :: Pos -> Command -> Pos
navigate (x, y, aim) ("forward", dis) = (x + dis, y + dis * aim, aim)
navigate (x, y, aim) ("down", dis) = (x, y, aim + dis)
navigate (x, y, aim) ("up", dis) = (x, y, aim - dis)

computeResult :: Pos -> Int
computeResult (x, y, _) = x * y