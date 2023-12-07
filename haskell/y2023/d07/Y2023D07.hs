module Main where

import Data.List (elem, group, nub, sort, sortOn)
import Data.List.Split (splitOn)
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 7
  let hands = map parseHand lines
  let p1 = part1 hands
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 hands
  putStrLn ("Part 2: " ++ show p2)

data Hand = MkHand String Int
  deriving (Show)

parseHand :: String -> Hand
parseHand s = MkHand cards (read bid)
  where
    [cards, bid] = splitOn " " s

part1 :: [Hand] -> Int
part1 hands = sum [pos * bid | (pos, x@(MkHand _ bid)) <- zip [1 .. (length hands)] orderedHands]
  where
    orderedHands = sortOn strength hands

strength :: Hand -> Int
strength (MkHand cards _) = handStrength cards + cardsStrength cardStrength cards

handStrength :: String -> Int
handStrength cards
  | length groupedCards == 1 = 6 * 13 ^ 5
  | any (\g -> length g == 4) groupedCards = 5 * 13 ^ 5
  | all (\g -> length g == 3 || length g == 2) groupedCards = 4 * 13 ^ 5
  | any (\g -> length g == 3) groupedCards = 3 * 13 ^ 5
  | length (filter (\g -> length g == 2) groupedCards) == 2 = 2 * 13 ^ 5
  | any (\g -> length g == 2) groupedCards = 13 ^ 5
  | otherwise = 0
  where
    groupedCards = group $ sort cards

cardsStrength :: (Char -> Int) -> [Char] -> Int
cardsStrength strength = foldl (\res x -> res * 13 + strength x) 0

cardStrength :: Char -> Int
cardStrength 'A' = 12
cardStrength 'K' = 11
cardStrength 'Q' = 10
cardStrength 'J' = 9
cardStrength 'T' = 8
cardStrength c = read [c] - 2

---------------------------

part2 :: [Hand] -> Int
part2 hands = sum [pos * bid | (pos, x@(MkHand _ bid)) <- zip [1 .. (length hands)] orderedHands]
  where
    orderedHands = sortOn strengthWithJoker hands

strengthWithJoker :: Hand -> Int
strengthWithJoker (MkHand cards _)
  | cards /= "JJJJJ" && 'J' `elem` cards = handStrengthWithJoker cards + cardsStrength cardStrengthWithJoker cards
  | otherwise = handStrength cards + cardsStrength cardStrengthWithJoker cards

handStrengthWithJoker :: [Char] -> Int
handStrengthWithJoker cards = maximum [handStrength (replaceJokers card cards) | card <- nub (filter (/= 'J') cards)]

replaceJokers :: Char -> [Char] -> [Char]
replaceJokers c = map (\x -> if x == 'J' then c else x)

cardStrengthWithJoker :: Char -> Int
cardStrengthWithJoker 'A' = 12
cardStrengthWithJoker 'K' = 11
cardStrengthWithJoker 'Q' = 10
cardStrengthWithJoker 'J' = 0
cardStrengthWithJoker 'T' = 9
cardStrengthWithJoker c = read [c] - 1
