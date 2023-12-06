module Main where

import Data.List (elem, null)
import Data.List.Split (splitOn)
import Data.String (words)
import Input (readLinesSplitOnEmptyLine)

main :: IO ()
main = do
  chunks <- readLinesSplitOnEmptyLine 2023 5
  let seeds = parseSeeds . head . head $ chunks
  let maps = map parseMap $ tail chunks
  let p1 = part1 seeds maps
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 seeds maps
  putStrLn ("Part 2: " ++ show p2)

data AlmanacMap = MkAlmanacMap String String [AlmanacSubMap]
  deriving (Show)

data AlmanacSubMap = MkAlmanacSubMap Int Int Int Int
  deriving (Show)

parseSeeds :: String -> [Int]
parseSeeds s = map read $ splitOn " " $ drop 7 s

parseMap :: [String] -> AlmanacMap
parseMap lines = MkAlmanacMap from to subMaps
  where
    [from, to] = splitOn "-to-" $ head $ splitOn " " $ head lines
    subMaps = map parseSubMap $ tail lines

parseSubMap :: String -> AlmanacSubMap
parseSubMap s = MkAlmanacSubMap dest source (source + length - 1) length
  where
    [dest, source, length] = map read $ splitOn " " s

part1 :: [Int] -> [AlmanacMap] -> Int
part1 locs maps = minimum $ foldl applyMap locs maps

applyMap :: [Int] -> AlmanacMap -> [Int]
applyMap locs (MkAlmanacMap _ _ subMaps) = map (newLocation subMaps) locs

newLocation :: [AlmanacSubMap] -> Int -> Int
newLocation [] x = x
newLocation (MkAlmanacSubMap dest start end _ : subMaps) x
  | distance >= 0 && x <= end = dest + distance
  | otherwise = newLocation subMaps x
  where
    distance = x - start

part2 :: [Int] -> [AlmanacMap] -> Int
part2 nums maps = minimum [n | x@(MkRange n _) <- newRanges]
  where
    myRanges = ranges nums
    newRanges = foldl applyMapRanges myRanges maps

data Range = MkRange Int Int
  deriving (Show)

ranges :: [Int] -> [Range]
ranges [] = []
ranges (source : length : nums) = MkRange source (source + length - 1) : ranges nums

applyMapRanges :: [Range] -> AlmanacMap -> [Range]
applyMapRanges ranges (MkAlmanacMap _ _ subMaps) = concatMap (newLocationOnRange subMaps) ranges

newLocationOnRange :: [AlmanacSubMap] -> Range -> [Range]
newLocationOnRange [] range = [range]
newLocationOnRange (MkAlmanacSubMap dest startM endM length : subMaps) (MkRange startR endR)
  | endM < startR || endR < startM = newLocationOnRange subMaps (MkRange startR endR) -- No overlap
  | startM <= startR && endR <= endM = [MkRange (startR + dest - startM) (endR + dest - startM)] -- Fully contained
  | startM > startR && endR > endM = MkRange dest (dest + length - 1) : newLocationOnRange subMaps (MkRange startR (startM - 1)) ++ newLocationOnRange subMaps (MkRange (endM + 1) endR) -- Hang over both sides
  | startM <= startR = MkRange (startR + dest - startM) (dest + length - 1) : newLocationOnRange subMaps (MkRange (endM + 1) endR) -- Hang over right
  | otherwise = MkRange dest (endR + dest - startM) : newLocationOnRange subMaps (MkRange startR (startM - 1)) -- Hang over left