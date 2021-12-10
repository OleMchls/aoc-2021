module Day10 where

prepare :: String -> [String]
prepare = lines

solve :: [String] -> Int
solve = sum . map score . map parse

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score '_' = 0

parse :: String -> Char
parse [] = '_'
parse ('(':xs) = findClose [')'] xs
parse ('[':xs) = findClose [']'] xs
parse ('{':xs) = findClose ['}'] xs
parse ('<':xs) = findClose ['>'] xs
parse (x:xs) = '_'

findClose :: [Char] -> [Char] -> Char
findClose x [] = '_'
findClose [] x = parse x
findClose (close:cs) (x:xs)
    | x == close = findClose cs xs
    | x == '('   = findClose (')':close:cs) xs
    | x == '['   = findClose (']':close:cs) xs
    | x == '{'   = findClose ('}':close:cs) xs
    | x == '<'   = findClose ('>':close:cs) xs
    | otherwise  = x
