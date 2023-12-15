module Main where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, foldlWithKey, insert, mapWithKey, member)
import Input (readSplitOn)

main :: IO ()
main = do
  steps <- readSplitOn 2023 15 ','
  p1 <- part1 steps
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 steps
  putStrLn ("Part 2: " ++ show p2)

part1 :: [String] -> IO Int
part1 steps = do
  let values = map (calcStep 0) steps
  return (sum values)

calcStep :: Int -> String -> Int
calcStep = foldl (\v c -> ((v + ord c) * 17) `mod` 256)

------------------------------------------

data Step
  = Remove Int String -- box label
  | Insert Int String Int -- box label lens
  deriving (Show)

parseStep :: String -> Step
parseStep s
  | last s == '-' = Remove (calcStep 0 (init s)) (init s)
  | otherwise = Insert (calcStep 0 (head split)) (head split) (read (last split))
  where
    split = splitOn "=" s

part2 :: [String] -> IO Int
part2 steps = do
  let parsedSteps = map parseStep steps
  let boxes = performSteps parsedSteps
  let power = foldlWithKey (\pow boxId lenses -> pow + calculateBoxPower 1 boxId lenses) 0 boxes
  return power

performSteps :: [Step] -> Map Int [(String, Int)]
performSteps = foldl performStep empty

performStep :: Map Int [(String, Int)] -> Step -> Map Int [(String, Int)]
performStep boxes (Remove box label) = mapWithKey (\k v -> if k == box then filter ((/= label) . fst) v else v) boxes
performStep boxes (Insert box label lens) = mapWithKey (\k v -> if k == box then insertLens v (label, lens) else v) (createBoxIfMissing boxes box)

createBoxIfMissing :: Map Int [(String, Int)] -> Int -> Map Int [(String, Int)]
createBoxIfMissing boxes boxId
  | member boxId boxes = boxes
  | otherwise = insert boxId [] boxes

insertLens :: [(String, Int)] -> (String, Int) -> [(String, Int)]
insertLens box lens@(label, value)
  | null after = before ++ [lens]
  | otherwise = before ++ [lens] ++ tail after
  where
    (before, after) = span ((/= label) . fst) box

calculateBoxPower :: Int -> Int -> [(String, Int)] -> Int
calculateBoxPower _ _ [] = 0
calculateBoxPower index boxId ((_, focalLength) : lenses) = (boxId + 1) * index * focalLength + calculateBoxPower (index + 1) boxId lenses