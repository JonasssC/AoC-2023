module Main where

import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 1
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

part1 :: [String] -> IO Int
part1 _ = do
  return 0

part2 :: [String] -> IO Int
part2 _ = do
  return 0
