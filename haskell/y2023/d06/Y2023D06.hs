module Main where

import Data.List (elem, null)
import Data.List.Split (splitOn)
import Data.String (words)
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 6
  let races = parseRaces lines
  let p1 = part1 races
  putStrLn ("Part 1: " ++ show p1)
  let race = parseRace lines
  let p2 = countWinning race
  putStrLn ("Part 2: " ++ show p2)

data Race = MkRace Int Int
  deriving (Show)

parseRaces :: [String] -> [Race]
parseRaces [timesS, distancesS] = parseRacesHelper times distances
  where
    times = map read $ tail $ words timesS
    distances = map read $ tail $ words distancesS

parseRacesHelper :: [Int] -> [Int] -> [Race]
parseRacesHelper [] [] = []
parseRacesHelper (time : times) (distance : distances) = MkRace time distance : parseRacesHelper times distances

part1 :: [Race] -> Int
part1 races = product $ map countWinning races

countWinning :: Race -> Int
countWinning (MkRace time distance) = length winning
  where
    speeds = [0 .. time]
    distances = map (\speed -> speed * (time - speed)) speeds
    winning = filter (> distance) distances

---------------------------

parseRace :: [String] -> Race
parseRace [timeS, distanceS] = MkRace time distance
  where
    time = read $ filter (/= ' ') $ drop 9 timeS
    distance = read $ filter (/= ' ') $ drop 9 distanceS
