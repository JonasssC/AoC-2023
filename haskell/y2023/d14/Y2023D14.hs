module Main where

import Data.List (elemIndex, sort, sortOn, (\\))
import Data.Maybe (fromJust, isNothing)
import Data.Ord
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 14
  let solidRocks = collectSolidRocks lines
  let roundRocks = collectRoundRocks lines
  let fieldSize = (length lines, length (head lines))
  p1 <- part1 fieldSize solidRocks roundRocks
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 fieldSize solidRocks roundRocks
  putStrLn ("Part 2: " ++ show p2)

type Coord = (Int, Int) -- (Row, Col)

collectSolidRocks :: [[Char]] -> [Coord]
collectSolidRocks = collectRocksHelper (== '#') (0, 0)

collectRoundRocks :: [[Char]] -> [Coord]
collectRoundRocks = collectRocksHelper (== 'O') (0, 0)

collectRocksHelper :: (Char -> Bool) -> Coord -> [[Char]] -> [Coord]
collectRocksHelper _ _ [] = []
collectRocksHelper check (row, _) ([] : lines) = collectRocksHelper check (row + 1, 0) lines
collectRocksHelper check coord@(row, col) ((c : line) : lines)
  | check c = coord : collectRocksHelper check (row, col + 1) (line : lines)
  | otherwise = collectRocksHelper check (row, col + 1) (line : lines)

part1 :: Coord -> [Coord] -> [Coord] -> IO Int
part1 fieldSize@(rowCount, _) occupied balls = do
  let rolledBalls = rollAll rollNorth fieldSize occupied (sortOn fst balls)
  let scores = map (\c -> rowCount - fst c) rolledBalls
  return (sum scores)

------------------------------------------

part2 :: Coord -> [Coord] -> [Coord] -> IO Int
part2 fieldSize@(rowCount, colCount) occupied balls = do
  let rolledBalls = cycleNTimes fieldSize 1000000000 [balls] occupied balls
  let scores = map (\c -> rowCount - fst c) rolledBalls
  return (sum scores)

cycleNTimes :: Coord -> Int -> [[Coord]] -> [Coord] -> [Coord] -> [Coord]
cycleNTimes _ 0 _ _ balls = balls
cycleNTimes fieldSize n patterns occupied balls
  | isNothing match = cycleNTimes fieldSize (n - 1) (cycled : patterns) occupied cycled
  | otherwise = simplifyRemainingCycles fieldSize (n - 1) (fromJust match) occupied cycled
  where
    cycled = cycleBalls fieldSize occupied balls
    match = elemIndex cycled patterns

simplifyRemainingCycles :: Coord -> Int -> Int -> [Coord] -> [Coord] -> [Coord]
simplifyRemainingCycles fieldSize n i = cycleNTimes fieldSize r []
  where
    r = n `mod` (i + 1)

cycleBalls :: Coord -> [Coord] -> [Coord] -> [Coord]
cycleBalls fieldSize occupied balls = sort rolledBallsEast
  where
    rolledBallsNorth = rollAll rollNorth fieldSize occupied (sortOn fst balls)
    rolledBallsWest = rollAll rollWest fieldSize occupied (sortOn snd rolledBallsNorth)
    rolledBallsSouth = rollAll rollSouth fieldSize occupied (sortOn (Data.Ord.Down . fst) rolledBallsWest)
    rolledBallsEast = rollAll rollEast fieldSize occupied (sortOn (Data.Ord.Down . snd) rolledBallsSouth)

type RollFunction = Coord -> [Coord] -> Coord -> Coord

rollAll :: RollFunction -> Coord -> [Coord] -> [Coord] -> [Coord]
rollAll f _ _ [] = []
rollAll f fieldSize occupied (ball : balls) = rolledBall : rollAll f fieldSize (rolledBall : occupied) balls
  where
    rolledBall = f fieldSize occupied ball

rollNorth :: RollFunction
rollNorth _ occupied (row, col) = (newRow, col)
  where
    blocking = sortOn fst . filter (\(rowX, colX) -> col == colX && rowX < row) $ occupied
    newRow = if null blocking then 0 else fst (last blocking) + 1

rollWest :: RollFunction
rollWest _ occupied (row, col) = (row, newCol)
  where
    blocking = sortOn snd . filter (\(rowX, colX) -> rowX == row && colX < col) $ occupied
    newCol = if null blocking then 0 else snd (last blocking) + 1

rollSouth :: RollFunction
rollSouth (rows, _) occupied (row, col) = (newRow, col)
  where
    blocking = sortOn fst . filter (\(rowX, colX) -> col == colX && rowX > row) $ occupied
    newRow = if null blocking then rows - 1 else fst (head blocking) - 1

rollEast :: RollFunction
rollEast (_, cols) occupied (row, col) = (row, newCol)
  where
    blocking = sortOn snd . filter (\(rowX, colX) -> rowX == row && colX > col) $ occupied
    newCol = if null blocking then cols - 1 else snd (head blocking) - 1
