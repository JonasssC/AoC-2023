module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map (Map, fromList, keys, (!))
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 1
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

part1 :: [String] -> IO Int
part1 lines = do
  let nums = map (toNumber . digits) lines
  return (sum nums)

digits :: String -> String
digits = filter isDigit

toNumber :: String -> Int
toNumber s = read (head s : [last s])

part2 :: [String] -> IO Int
part2 lines = do
  let nums = map (\line -> startingNumber line * 10 + endingNumber line) lines
  return (sum nums)

numbers :: Map String Int
numbers = fromList [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

startingNumber :: String -> Int
startingNumber s
  | isDigit (head s) = read [head s]
  | null matches = startingNumber (tail s)
  | otherwise = numbers ! head matches
  where
    matches = filter (`isPrefixOf` s) (keys numbers)

endingNumber :: String -> Int
endingNumber s
  | isDigit (last s) = read [last s]
  | null matches = endingNumber (init s)
  | otherwise = numbers ! head matches
  where
    matches = filter (`isSuffixOf` s) (keys numbers)
