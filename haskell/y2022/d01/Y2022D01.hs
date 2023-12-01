module Main where

import Data.List (sort)
import Input (readLinesSplitOnEmptyLineAs)

main :: IO ()
main = do
  lines <- readLinesSplitOnEmptyLineAs 2022 1 :: IO [[Int]]
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

part1 :: [[Int]] -> IO Int
part1 elfs = do
  let calories = map sum elfs
  return (maximum calories)

part2 :: [[Int]] -> IO Int
part2 elfs = do
  let calories = map sum elfs
  let top3Calories = take 3 . reverse . sort $ calories
  return (sum top3Calories)
