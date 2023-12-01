module Main where

import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2022 2
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

part1 :: [String] -> IO Int
part1 games = do
  let scores = map score games
  return (sum scores)

score :: String -> Int
score (op : _ : self : _)
  | won op self = 6 + point self
  | lost op self = 0 + point self
  | otherwise = 3 + point self

won :: Char -> Char -> Bool
won 'A' 'Y' = True
won 'B' 'Z' = True
won 'C' 'X' = True
won _ _ = False

lost :: Char -> Char -> Bool
lost 'A' 'Z' = True
lost 'B' 'X' = True
lost 'C' 'Y' = True
lost _ _ = False

point :: Char -> Int
point 'X' = 1
point 'Y' = 2
point 'Z' = 3

part2 :: [String] -> IO Int
part2 games = do
  let scores = map score2 games
  return (sum scores)

score2 :: String -> Int
score2 (op : _ : outcome : _) =
  case outcome of
    'X' -> 0 + (point . losesTo $ op)
    'Y' -> 3 + (point . tiesTo $ op)
    'Z' -> 6 + (point . winsFrom $ op)

winsFrom :: Char -> Char
winsFrom 'A' = 'Y'
winsFrom 'B' = 'Z'
winsFrom 'C' = 'X'

losesTo :: Char -> Char
losesTo 'A' = 'Z'
losesTo 'B' = 'X'
losesTo 'C' = 'Y'

tiesTo :: Char -> Char
tiesTo 'A' = 'X'
tiesTo 'B' = 'Y'
tiesTo 'C' = 'Z'
