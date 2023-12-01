module Main where

import Data.Int (Int)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String (String)
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
digits = filter (<= '9')

toNumber :: String -> Int
toNumber s = read (head s : [last s])

part2 :: [String] -> IO Int
part2 lines = do
  let nums = map (\line -> startingNumber line * 10 + endingNumber line) lines
  return (sum nums)

startingNumber :: String -> Int
startingNumber s
  | "zero" `isPrefixOf` s = 0
  | "0" `isPrefixOf` s = 0
  | "one" `isPrefixOf` s = 1
  | "1" `isPrefixOf` s = 1
  | "two" `isPrefixOf` s = 2
  | "2" `isPrefixOf` s = 2
  | "three" `isPrefixOf` s = 3
  | "3" `isPrefixOf` s = 3
  | "four" `isPrefixOf` s = 4
  | "4" `isPrefixOf` s = 4
  | "five" `isPrefixOf` s = 5
  | "5" `isPrefixOf` s = 5
  | "six" `isPrefixOf` s = 6
  | "6" `isPrefixOf` s = 6
  | "seven" `isPrefixOf` s = 7
  | "7" `isPrefixOf` s = 7
  | "eight" `isPrefixOf` s = 8
  | "8" `isPrefixOf` s = 8
  | "nine" `isPrefixOf` s = 9
  | "9" `isPrefixOf` s = 9
  | otherwise = startingNumber (tail s)

endingNumber :: String -> Int
endingNumber s
  | "zero" `isSuffixOf` s = 0
  | "0" `isSuffixOf` s = 0
  | "one" `isSuffixOf` s = 1
  | "1" `isSuffixOf` s = 1
  | "two" `isSuffixOf` s = 2
  | "2" `isSuffixOf` s = 2
  | "three" `isSuffixOf` s = 3
  | "3" `isSuffixOf` s = 3
  | "four" `isSuffixOf` s = 4
  | "4" `isSuffixOf` s = 4
  | "five" `isSuffixOf` s = 5
  | "5" `isSuffixOf` s = 5
  | "six" `isSuffixOf` s = 6
  | "6" `isSuffixOf` s = 6
  | "seven" `isSuffixOf` s = 7
  | "7" `isSuffixOf` s = 7
  | "eight" `isSuffixOf` s = 8
  | "8" `isSuffixOf` s = 8
  | "nine" `isSuffixOf` s = 9
  | "9" `isSuffixOf` s = 9
  | otherwise = endingNumber (init s)
