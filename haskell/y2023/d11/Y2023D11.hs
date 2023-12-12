module Main where

import Data.List (elem, sort, transpose, union, (\\))
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 11
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

type Coord = (Int, Int)

part1 :: [[Char]] -> IO Int
part1 image = do
  let expandedImage = expandImage image
  let galaxies = findGalaxies expandedImage
  let distances = calculateDistances galaxies
  return (sum distances)

expandImage :: [[Char]] -> [[Char]]
expandImage = transpose . expandRows . transpose . expandRows

expandRows :: [[Char]] -> [[Char]]
expandRows [] = []
expandRows (row : image)
  | all (== '.') row = row : row : expandRows image
  | otherwise = row : expandRows image

findGalaxies :: [[Char]] -> [Coord]
findGalaxies = findGalaxiesHelper (0, 0)

findGalaxiesHelper :: Coord -> [[Char]] -> [Coord]
findGalaxiesHelper _ [] = []
findGalaxiesHelper (_, y) ([] : rows) = findGalaxiesHelper (0, y + 1) rows
findGalaxiesHelper (x, y) ((char : row) : rows)
  | char == '#' = (x, y) : findGalaxiesHelper (x + 1, y) (row : rows)
  | otherwise = findGalaxiesHelper (x + 1, y) (row : rows)

calculateDistances :: [Coord] -> [Int]
calculateDistances galaxies = [calculateDistance ((x1, y1), (x2, y2)) | (x1, y1) <- galaxies, (x2, y2) <- galaxies, y2 > y1 || (y2 == y1 && x2 > x1)]

calculateDistance :: (Coord, Coord) -> Int
calculateDistance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

---------------------------------------------------------

part2 :: [[Char]] -> IO (Int, Int)
part2 image = do
  let galaxies = findGalaxies image
  let emptyRowIndices = emptyRows image
  let emptyColIndices = emptyColumns image
  let distances = calculateDistances2 emptyRowIndices emptyColIndices galaxies
  let summed = foldl (\(mils, ones) (milsX, onesX) -> (mils + milsX, ones + onesX)) (0, 0) distances
  return summed

emptyRows :: [[Char]] -> [Int]
emptyRows = emptyRowsHelper 0

emptyRowsHelper :: Int -> [[Char]] -> [Int]
emptyRowsHelper _ [] = []
emptyRowsHelper i (row : rows)
  | all (== '.') row = i : emptyRowsHelper (i + 1) rows
  | otherwise = emptyRowsHelper (i + 1) rows

emptyColumns :: [[Char]] -> [Int]
emptyColumns = emptyRows . transpose

calculateDistances2 :: [Int] -> [Int] -> [Coord] -> [(Int, Int)]
calculateDistances2 emptyRowIndices emptyColIndices galaxies = [calculateDistance2 emptyRowIndices emptyColIndices ((x1, y1), (x2, y2)) | (x1, y1) <- galaxies, (x2, y2) <- galaxies, y2 > y1 || (y2 == y1 && x2 > x1)]

calculateDistance2 :: [Int] -> [Int] -> (Coord, Coord) -> (Int, Int)
calculateDistance2 emptyRowIndices emptyColIndices ((x1, y1), (x2, y2)) = (emptyRows + emptyColumns, nonEmptyRows + nonEmptyColumns)
  where
    [minY, maxY] = sort [y1, y2]
    [minX, maxX] = sort [x1, x2]
    emptyRows = length [y | y <- emptyRowIndices, y > y1 && y < y2]
    emptyColumns = length [x | x <- emptyColIndices, x > x1 && x < x2]
    nonEmptyRows = abs (y1 - y2) - emptyRows
    nonEmptyColumns = abs (x1 - x2) - emptyColumns