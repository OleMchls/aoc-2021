module Day4 where

import Data.List ( transpose, partition )

data BingoNum = BingoNum {
                  checked :: Bool,
                  value :: Int
                } deriving (Show)

type Board = [[BingoNum]]
type WonBoard = (Int, Board)
type State = ([WonBoard], [Board])
type Input = ([Int], [Board])

prepare :: String -> Input
prepare = parseInput . lines

parseInput :: [String] -> Input
parseInput (numbers : boards) = (parseNumbers numbers, parseBoards boards [])

parseBoards :: [String] -> [Board] -> [Board]
parseBoards [] boards = reverse boards
parseBoards ("":xs) boards = parseBoards rest (parseBoard current:boards)
    where (current, rest) = break (== "") xs

parseBoard :: [String] -> Board
parseBoard = map (map (toBingoNum . read) . words)
    where toBingoNum v = BingoNum {checked = False, value = v}

parseNumbers :: String -> [Int]
parseNumbers numbers = map read (words [if c == ',' then ' ' else c | c <- numbers])

solve :: Input -> Int
solve (numbers, boards) = calulateScore (lastWinner finalState)
    where winningState = last $ takeWhileInclusive (not . completed) yieldStates
          finalState = foldl applyNumber ([], boards) numbers
          yieldStates = scanl applyNumber ([], boards) numbers
          completed ([], _) = False
          completed _ = True
          firstWinner (winners, _) = head winners
          lastWinner (winners, _) = last winners

applyNumber :: State -> Int -> State
applyNumber (wonBoards, boards) number = appendState number wonBoards (partition boardWon updatedBoards)
    where updatedBoards = map (checkOffNumber number) boards

appendState :: Int -> [WonBoard] -> ([Board], [Board]) -> State
appendState number wonBoards (newWon, rest) = (wonBoards ++ toWonBoard newWon, rest)
    where toWonBoard = map (\b -> (number, b))

checkOffNumber :: Int -> Board -> Board
checkOffNumber num board = [[bnum {checked = (num == value bnum) || checked bnum} | bnum <- rows] | rows <- board]

findWinners :: [Board] -> [Board]
findWinners = filter boardWon

boardWon :: Board -> Bool
boardWon board = any (all checked) (board ++ transpose board)

calulateScore :: WonBoard -> Int
calulateScore (n, board) = n * sum (map value (filter (not . checked) numbers))
    where numbers = concat board

-- There must be a better way to yield States until condition matches
-- https://stackoverflow.com/questions/22472536/does-haskell-have-a-takeuntil-function
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x :
  if p x
    then takeWhileInclusive p xs
    else []
