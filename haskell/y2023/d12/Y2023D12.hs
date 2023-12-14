module Main where

import Data.List (elem, group, sort, transpose, union, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map, filterWithKey, fromList, fromListWith, mapKeysWith, toList)
import Debug.Trace (trace, traceShowId)
import Input (readLines)

main :: IO ()
main = do
  lines <- readLines 2023 12
  let parsedLines = map parse lines
  p1 <- part1 parsedLines
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 parsedLines
  putStrLn ("Part 2: " ++ show p2)

data Line = MkLine String [Int]
  deriving (Show)

parse :: String -> Line
parse line = MkLine s (map read $ splitOn "," nums)
  where
    [s, nums] = splitOn " " line

part1 :: [Line] -> IO Int
part1 lines = do
  counts <- countPossibles lines
  return (sum counts)

------------------------------------------

part2 :: [Line] -> IO Int
part2 lines = do
  let unfolded = map unfoldLine lines
  counts <- countPossibles unfolded
  return (sum counts)

unfoldLine :: Line -> Line
unfoldLine (MkLine s nums) = MkLine (s ++ "?" ++ s ++ "?" ++ s ++ "?" ++ s ++ "?" ++ s) (nums ++ nums ++ nums ++ nums ++ nums)

countPossibles :: [Line] -> IO [Int]
countPossibles [] = do return []
countPossibles (line : lines) = do
  let c = countPossible line
  cs <- countPossibles lines
  return (c : cs)

countPossible :: Line -> Int
countPossible line = sum (countPossibleHelper line (fromList [((0, 0), 1)]))

type State = (Int, Int) -- (group, amount)

countPossibleHelper :: Line -> Map State Int -> Map State Int
countPossibleHelper (MkLine [] nums) states = filterWithKey (\k _ -> isFinished nums k) states
countPossibleHelper (MkLine (c : line) nums) states = countPossibleHelper (MkLine line nums) . fromListWith (+) . concatMap (expandState c nums) . toList $ states

expandState :: Char -> [Int] -> (State, Int) -> [(State, Int)]
expandState '.' nums ((group, amount), count)
  | amount == 0 = [((group, 0), count)]
  | group < length nums && amount == nums !! group = [((group + 1, 0), count)]
  | otherwise = []
expandState '#' nums ((group, amount), count)
  | group < length nums && amount < nums !! group = [((group, amount + 1), count)]
  | otherwise = []
expandState '?' nums state = expandState '.' nums state ++ expandState '#' nums state

isFinished :: [Int] -> State -> Bool
isFinished nums (group, amount) = length nums == group || (length nums == group + 1 && amount == last nums)