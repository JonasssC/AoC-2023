module Main where

import Data.String (words)
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 9
  let values = map parseLine lines
  let p1 = part1 values
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 values
  putStrLn ("Part 2: " ++ show p2)

parseLine :: String -> [Int]
parseLine s = map read $ words s

part1 :: [[Int]] -> Int
part1 sequences = sum [nextInSequence sequence | sequence <- sequences]

nextInSequence :: [Int] -> Int
nextInSequence sequence
  | all (== 0) sequence = 0
  | otherwise = last sequence + nextInSequence (diffSequence sequence)

diffSequence :: [Int] -> [Int]
diffSequence sequence = [sequence !! i - sequence !! (i - 1) | i <- [1 .. (length sequence - 1)]]

---------------------------

part2 :: [[Int]] -> Int
part2 sequences = sum [prevInSequence sequence | sequence <- sequences]

prevInSequence :: [Int] -> Int
prevInSequence sequence
  | all (== 0) sequence = 0
  | otherwise = head sequence - prevInSequence (diffSequence sequence)
