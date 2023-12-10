module Main where

import Data.List (elem, union, (\\))
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 10
  let path = collectPath lines
  let s = sType lines $ findS lines
  innerCount <- countInner s lines path
  putStrLn ("Part 1: " ++ show (fromIntegral (length path) / 2))
  putStrLn ("Part 2: " ++ show innerCount)

type Coord = (Int, Int)

collectPath :: [[Char]] -> [Coord]
collectPath field = collectPathHelper field [s] start
  where
    s = findS field
    [start, _] = findStartingNeighbours s field

collectPathHelper :: [[Char]] -> [Coord] -> Coord -> [Coord]
collectPathHelper field visited coord
  | null nextCoord = coord : visited
  | otherwise = collectPathHelper field (coord : visited) (head nextCoord)
  where
    visitable = findNextNeighbours coord field
    nextCoord = visitable \\ visited

findS :: [[Char]] -> Coord
findS = findSHelper (0, 0)

findSHelper :: Coord -> [[Char]] -> Coord
findSHelper (x, y) ([] : lines) = findSHelper (0, y + 1) lines
findSHelper (x, y) ((c : line) : lines)
  | c == 'S' = (x, y)
  | otherwise = findSHelper (x + 1, y) (line : lines)

findStartingNeighbours :: Coord -> [[Char]] -> [Coord]
findStartingNeighbours (x, y) field = up ++ right ++ down ++ left
  where
    up = [(x, y - 1) | charAtCoord field (x, y - 1) `elem` ['|', '7', 'F']]
    right = [(x + 1, y) | charAtCoord field (x + 1, y) `elem` ['-', '7', 'J']]
    down = [(x, y + 1) | charAtCoord field (x, y + 1) `elem` ['|', 'J', 'L']]
    left = [(x - 1, y) | charAtCoord field (x - 1, y) `elem` ['-', 'L', 'F']]

findNextNeighbours :: Coord -> [[Char]] -> [Coord]
findNextNeighbours (x, y) field = case char of
  '|' -> [(x, y - 1), (x, y + 1)]
  '-' -> [(x - 1, y), (x + 1, y)]
  'L' -> [(x, y - 1), (x + 1, y)]
  'J' -> [(x, y - 1), (x - 1, y)]
  '7' -> [(x, y + 1), (x - 1, y)]
  'F' -> [(x, y + 1), (x + 1, y)]
  where
    char = charAtCoord field (x, y)

charAtCoord :: [[Char]] -> Coord -> Char
charAtCoord field (x, y)
  | x < 0 || y < 0 || x >= width || y >= height = '.'
  | otherwise = field !! y !! x
  where
    (width, height) = fieldDimensions field

fieldDimensions :: [[Char]] -> Coord
fieldDimensions (row : field) = (length row, length (row : field))

---------------------------

countInner :: Char -> [[Char]] -> [Coord] -> IO Int
countInner = countInnerHelper 0

countInnerHelper :: Int -> Char -> [[Char]] -> [Coord] -> IO Int
countInnerHelper _ s [] _ = do return 0
countInnerHelper column s (row : field) path = do
  rowCount <- countInnerRow s row path (False, True) (0, column)
  putStrLn ""
  restCount <- countInnerHelper (column + 1) s field path
  return (rowCount + restCount)

countInnerRow :: Char -> [Char] -> [Coord] -> (Bool, Bool) -> Coord -> IO Int
countInnerRow s [] _ _ _ = do return 0
countInnerRow s (char : row) path (inside, up) (x, y)
  | char == 'S' = do
      putStr [s]
      countInnerRow s row path (switchSide s (inside, up)) (x + 1, y)
  | (x, y) `elem` path = do
      putStr [char]
      countInnerRow s row path (switchSide char (inside, up)) (x + 1, y)
  | inside = do
      putStr "\ESC[96mX\ESC[0m"
      count <- countInnerRow s row path (inside, up) (x + 1, y)
      return (1 + count)
  | otherwise = do
      putStr " "
      countInnerRow s row path (inside, up) (x + 1, y)

sType :: [[Char]] -> Coord -> Char
sType field (x, y)
  | x1 == x + 1 && y2 == y + 1 = 'F'
  | x1 == x + 1 && y2 == y - 1 = 'L'
  | x1 == x + 1 && x2 == x - 1 = '-'
  | x1 == x - 1 && y2 == y + 1 = '7'
  | x1 == x - 1 && y2 == y - 1 = 'J'
  | x1 == x - 1 && x2 == x + 1 = '-'
  | y1 == y + 1 && y2 == y - 1 = '|'
  | y1 == y + 1 && x2 == x + 1 = 'F'
  | y1 == y + 1 && x2 == x - 1 = '7'
  | y1 == y - 1 && y2 == y + 1 = '|'
  | y1 == y - 1 && y2 == x + 1 = 'L'
  | y1 == y - 1 && x2 == x - 1 = 'J'
  where
    [(x1, y1), (x2, y2)] = findStartingNeighbours (x, y) field

switchSide :: Char -> (Bool, Bool) -> (Bool, Bool)
switchSide char (inside, up) = case char of
  '|' -> (not inside, True)
  '7' -> if up then (inside, True) else (not inside, False)
  'J' -> if up then (not inside, True) else (inside, False)
  'F' -> (not inside, False)
  'L' -> (not inside, True)
  _ -> (inside, up)
