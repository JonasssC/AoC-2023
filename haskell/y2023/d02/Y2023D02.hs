module Main where

import Data.List.Split (splitOn)
import Input (readLines)
import System.TimeIt

main :: IO ()
main = do
  lines <- readLines 2023 2
  p1 <- timeItNamed "Part 1" $ part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- timeItNamed "Part 2" $ part2 lines
  putStrLn ("Part 2: " ++ show p2)

data Game = MkGame Int [[Reveal]]
  deriving (Show)

data Reveal = Red Int | Blue Int | Green Int
  deriving (Show)

part1 :: [String] -> IO Int
part1 lines = do
  let games = map parseGame lines
  let validGames = filter isValidGame games
  let ids = map getId validGames
  return (sum ids)

getId :: Game -> Int
getId (MkGame id _) = id

parseGame :: String -> Game
parseGame s = MkGame id reveals
  where
    split = splitOn ":" s
    headS = drop 4 (head split)
    revealsS = last split
    id = read headS :: Int
    pulls = splitOn ";" revealsS
    reveals = map parseReveals pulls

parseReveals :: String -> [Reveal]
parseReveals s = map parseReveal pulls
  where
    pulls = splitOn "," s

parseReveal :: String -> Reveal
parseReveal s
  | colour == "blue" = Blue n
  | colour == "red" = Red n
  | colour == "green" = Green n
  | otherwise = error "invalid colour"
  where
    [_, nS, colour] = splitOn " " s
    n = read nS :: Int

isValidGame :: Game -> Bool
isValidGame (MkGame _ reveals) = all isValidReveal reveals

isValidReveal :: [Reveal] -> Bool
isValidReveal [] = True
isValidReveal (Red n : r) = n <= 12 && isValidReveal r
isValidReveal (Green n : r) = n <= 13 && isValidReveal r
isValidReveal (Blue n : r) = n <= 14 && isValidReveal r

part2 :: [String] -> IO Int
part2 lines = do
  let games = map parseGame lines
  let powers = map getPower games
  return (sum powers)

getPower :: Game -> Int
getPower (MkGame _ reveals) = maxRed * maxBlue * maxGreen
  where
    flatReveals = concat reveals
    maxRed = maximum [n | x@(Red n) <- flatReveals]
    maxBlue = maximum [n | x@(Blue n) <- flatReveals]
    maxGreen = maximum [n | x@(Green n) <- flatReveals]
