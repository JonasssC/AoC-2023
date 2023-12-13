module Main where

import Data.List (elemIndices, findIndices, transpose)
import Input (readLinesSplitOnEmptyLine)

main :: IO ()
main = do
  patterns <- readLinesSplitOnEmptyLine 2023 13
  p1 <- part1 patterns
  putStrLn ("Part 1: " ++ show p1)
  p2 <- part2 patterns
  putStrLn ("Part 2: " ++ show p2)

part1 :: [[String]] -> IO Int
part1 patterns = do
  let scores = map mirrorScore patterns
  return (sum scores)

mirrorScore :: [String] -> Int
mirrorScore pattern = mirrored2
  where
    mirrored = horizontalMirror pattern
    mirrored2 = if mirrored == 0 then verticalMirror pattern else 100 * mirrored

verticalMirror :: [String] -> Int
verticalMirror = horizontalMirror . transpose

horizontalMirror :: [String] -> Int
horizontalMirror pattern = mirrored2
  where
    mirrored = horizontalMirrorStart pattern
    mirrored2 = if mirrored == 0 then horizontalMirrorEnd pattern else mirrored

horizontalMirrorStart :: [String] -> Int
horizontalMirrorStart pattern
  | null validMatches = 0
  | otherwise = (head validMatches + 1) `div` 2
  where
    matches = filter (> 0) $ elemIndices (head pattern) pattern
    validMatches = filter (\x -> isMirrored $ take (x + 1) pattern) matches

horizontalMirrorEnd :: [String] -> Int
horizontalMirrorEnd pattern
  | revMatch == 0 = 0
  | otherwise = length pattern - revMatch
  where
    revMatch = horizontalMirrorStart $ reverse pattern

isMirrored :: [String] -> Bool
isMirrored s = s == reverse s

------------------------------------------

part2 :: [[String]] -> IO Int
part2 patterns = do
  let scores = map mirrorScore2 patterns
  -- print scores
  return (sum scores)

mirrorScore2 :: [String] -> Int
mirrorScore2 pattern = mirrored2
  where
    mirrored = horizontalMirror2 pattern
    mirrored2 = if mirrored == 0 then verticalMirror2 pattern else 100 * mirrored

verticalMirror2 :: [String] -> Int
verticalMirror2 = horizontalMirror2 . transpose

horizontalMirror2 :: [String] -> Int
horizontalMirror2 pattern = mirrored2
  where
    mirrored = horizontalMirrorStart2 pattern
    mirrored2 = if mirrored == 0 then horizontalMirrorEnd2 pattern else mirrored

horizontalMirrorStart2 :: [String] -> Int
horizontalMirrorStart2 pattern
  | null validMatches = 0
  | otherwise = (head validMatches + 1) `div` 2
  where
    first = head pattern
    matches = filter (> 0) $ findIndices (differsMaxOneChar first) pattern
    validMatches = filter (\x -> isMirroredExceptForOneChar $ take (x + 1) pattern) matches

horizontalMirrorEnd2 :: [String] -> Int
horizontalMirrorEnd2 pattern
  | revMatch == 0 = 0
  | otherwise = length pattern - revMatch
  where
    revMatch = horizontalMirrorStart2 $ reverse pattern

isMirroredExceptForOneChar :: [String] -> Bool
isMirroredExceptForOneChar pattern
  | length pattern `mod` 2 == 1 = False
  | isMirrored pattern = False
  | head pattern == last pattern = isMirroredExceptForOneChar $ init . tail $ pattern
  | equalExceptForOneChar (head pattern) (last pattern) = isMirrored $ init . tail $ pattern
  | otherwise = False

differsMaxOneChar :: String -> String -> Bool
differsMaxOneChar s1 s2 = length [1 | (c1, c2) <- zip s1 s2, c1 /= c2] < 2

equalExceptForOneChar :: String -> String -> Bool
equalExceptForOneChar s1 s2 = length [1 | (c1, c2) <- zip s1 s2, c1 /= c2] == 1
