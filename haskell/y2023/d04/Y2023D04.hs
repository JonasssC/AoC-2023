module Main where

import Data.List (elem, null)
import Data.List.Split (splitOn)
import Data.String (words)
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 4
  let cards = map parseCard lines
  p1 <- part1 cards
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 cards
  putStrLn ("Part 2: " ++ show p2)

data Card = MkCard Int [Int] [Int]
  deriving (Show)

parseCard :: String -> Card
parseCard s = MkCard id winning numbers
  where
    split = splitOn ":" s
    id = read (drop 4 (head split))
    numsSplit = splitOn "|" (last split)
    winning = map read (words (head numsSplit))
    numbers = map read (words (last numsSplit))

part1 :: [Card] -> IO Int
part1 cards = do
  let scores = map scoreCard cards
  return (sum scores)

scoreCard :: Card -> Int
scoreCard card
  | winCount == 0 = 0
  | otherwise = 2 ^ (winCount - 1)
  where
    winCount = winCountCard card

winCountCard :: Card -> Int
winCountCard (MkCard _ w n) = length (filter (`elem` w) n)

part2 :: [Card] -> IO Int
part2 cards = do
  let counts = map (const 1) cards
  return $ part2Helper cards counts

part2Helper :: [Card] -> [Int] -> Int
part2Helper [] [] = 0
part2Helper (card : cards) (count : counts) = count + part2Helper cards newCounts
  where
    winCount = winCountCard card
    newCounts = map (+ count) (take winCount counts) ++ drop winCount counts