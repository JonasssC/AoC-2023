module Main where

import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Map (Map, empty, findWithDefault, fromList, insert, (!))
import Input (readLines)

type Coord = (Int, Int)

type GearMap = Map Coord Int

main :: IO ()
main = do
  lines <- readLines 2023 3
  p1 <- part1 lines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 lines
  putStrLn ("Part 2: " ++ show p2)

part1 :: [[Char]] -> IO Int
part1 lines = do
  let (width, height) = dimensions lines
  return (part1Helper lines lines (0, 0) 0 [])

part1Helper :: [[Char]] -> [[Char]] -> Coord -> Int -> [Coord] -> Int
part1Helper m [[]] _ v s
  | v > 0 && any (isSymbol m) s = v
  | otherwise = 0
part1Helper m ([] : body) (_, y) v s
  | v > 0 && any (isSymbol m) s = v + part1Helper m body (0, y + 1) 0 []
  | otherwise = part1Helper m body (0, y + 1) 0 []
part1Helper m ((c : line) : body) (x, y) v s
  | isDigit c = part1Helper m (line : body) (x + 1, y) (v * 10 + read [c] :: Int) (s ++ surrounding (x, y))
  | v > 0 && any (isSymbol m) s = v + part1Helper m (line : body) (x + 1, y) 0 []
  | otherwise = part1Helper m (line : body) (x + 1, y) 0 []

dimensions :: [[Char]] -> Coord
dimensions m = (length (head m), length m)

isSymbol :: [[Char]] -> Coord -> Bool
isSymbol m (x, y) = inBounds m (x, y) && not ([(m !! y) !! x] `isInfixOf` ".0123456789")

inBounds :: [[Char]] -> Coord -> Bool
inBounds m (x, y) = x >= 0 && x < width && y >= 0 && y < height
  where
    (width, height) = dimensions m

surrounding :: Coord -> [Coord]
surrounding (x, y) = [(x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x, y + 1), (x, y - 1), (x - 1, y + 1), (x - 1, y), (x - 1, y - 1)]

part2 :: [[Char]] -> IO Int
part2 lines = do
  let (width, height) = dimensions lines
  return (part2Helper lines lines (0, 0) 0 [] empty)

part2Helper :: [[Char]] -> [[Char]] -> Coord -> Int -> [Coord] -> GearMap -> Int
part2Helper m [[]] _ v s g
  | v > 0 && not (null gears) = tryMultiply g v gears
  | otherwise = 0
  where
    gears = filter (isGear m) s
part2Helper m ([] : body) (_, y) v s g
  | v > 0 && not (null gears) = tryMultiply g v gears + part2Helper m body (0, y + 1) 0 [] (insertGears g v gears)
  | otherwise = part2Helper m body (0, y + 1) 0 [] g
  where
    gears = filter (isGear m) s
part2Helper m ((c : line) : body) (x, y) v s g
  | isDigit c = part2Helper m (line : body) (x + 1, y) (v * 10 + read [c] :: Int) (s ++ surrounding (x, y)) g
  | v > 0 && not (null gears) = tryMultiply g v gears + part2Helper m (line : body) (x + 1, y) 0 [] (insertGears g v gears)
  | otherwise = part2Helper m (line : body) (x + 1, y) 0 [] g
  where
    gears = filter (isGear m) s

isGear :: [[Char]] -> Coord -> Bool
isGear m (x, y) = inBounds m (x, y) && (m !! y) !! x == '*'

tryMultiply :: GearMap -> Int -> [Coord] -> Int
tryMultiply gearMap v [] = 0
tryMultiply gearMap v (gear : gears)
  | x == 0 = tryMultiply gearMap v gears
  | otherwise = x * v
  where
    x = findWithDefault 0 gear gearMap

insertGears :: GearMap -> Int -> [Coord] -> GearMap
insertGears m _ [] = m
insertGears m v (gear : gears) = insert gear v (insertGears m v gears)