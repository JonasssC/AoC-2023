module Input where

import Data.Text (replace, split)
import Text.Printf (printf)

readStr :: Int -> Int -> IO String
readStr y d = do
  let path = printf "./y%d/d%02d/input.txt" y d
  readFile path

readLines :: Int -> Int -> IO [String]
readLines y d = do
  input <- readStr y d
  return (lines input)

readLinesAs :: (Read a) => Int -> Int -> IO [a]
readLinesAs y d = do
  lines <- readLines y d
  return (map read lines)

readSplitOnEmptyLine :: Int -> Int -> IO [String]
readSplitOnEmptyLine y d = do
  input <- readStr y d
  let input2 = filter (/= '\r') input
  return (splitOnEmptyLine input2)

splitOnEmptyLine :: String -> [String]
splitOnEmptyLine "" = []
splitOnEmptyLine [x] = [[x]]
splitOnEmptyLine ('\n' : '\n' : rest) = "" : splitOnEmptyLine rest
splitOnEmptyLine (x : rest) = (x : word) : tail
  where
    (word : tail) = splitOnEmptyLine rest

readLinesSplitOnEmptyLine :: Int -> Int -> IO [[String]]
readLinesSplitOnEmptyLine y d = do
  parts <- readSplitOnEmptyLine y d
  return (map lines parts)

readLinesSplitOnEmptyLineAs :: (Read a) => Int -> Int -> IO [[a]]
readLinesSplitOnEmptyLineAs y d = do
  parts <- readLinesSplitOnEmptyLine y d
  return (map (map read) parts)

readSplitOn :: Int -> Int -> Char -> IO [String]
readSplitOn y d c = do
  input <- readStr y d
  return (splitOn c input)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn _ [x] = [[x]]
splitOn c (x : rest)
  | c == x = "" : (word : tail)
  | otherwise = (x : word) : tail
  where
    (word : tail) = splitOn c rest

readSplitOnAs :: (Read a) => Int -> Int -> Char -> IO [a]
readSplitOnAs y d c = do
  parts <- readSplitOn y d c
  return (map read parts)