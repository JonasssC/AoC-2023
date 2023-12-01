module Main where

import Data.Char (ord)
import Input (readByGroupedLines, readLines)

main :: IO ()
main = do
  lines <- readLines 2022 3
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  chunks <- readByGroupedLines 2022 3 3
  p2 <- part2 chunks
  putStrLn ("Part 2: " ++ show p2)

part1 :: [String] -> IO Int
part1 sacks = do
  let priorities = map sackPriority sacks
  return (sum priorities)

sackPriority :: String -> Int
sackPriority sack = priority (head [l | l <- left, r <- right, l == r])
  where
    (left, right) = splitString sack

splitString :: String -> (String, String)
splitString s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27

part2 :: [[String]] -> IO Int
part2 sacks = do
  let priorities = map sacksPriority sacks
  return (sum priorities)

sacksPriority :: [String] -> Int
sacksPriority (sack1 : sack2 : sack3 : _) = priority (head [s1 | s1 <- sack1, s2 <- sack2, s1 == s2, s3 <- sack3, s1 == s3])