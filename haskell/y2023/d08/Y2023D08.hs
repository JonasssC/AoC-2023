module Main where

import Data.List (isSuffixOf)
import Data.Map (Map, empty, findWithDefault, fromList, insert, keys, (!))
import Input (readLinesSplitOnEmptyLine)

main :: IO ()
main = do
  chunks <- readLinesSplitOnEmptyLine 2023 8
  let instructions = head $ head chunks
  let map = parseMap $ last chunks
  let p1 = part1 instructions map
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 instructions map
  putStrLn ("Part 2: " ++ show p2)

parseMap :: [String] -> Map String (String, String)
parseMap [] = empty
parseMap (line : lines) = insert key (left, right) (parseMap lines)
  where
    key = take 3 line
    left = take 3 $ drop 7 line
    right = take 3 $ drop 12 line

part1 :: String -> Map String (String, String) -> Int
part1 instr = stepCounter instr instr "AAA" "ZZZ"

stepCounter :: String -> String -> String -> String -> Map String (String, String) -> Int
stepCounter fullInstr [] loc endSuf m = stepCounter fullInstr fullInstr loc endSuf m
stepCounter fullInstr (instr : instrs) loc endSuf m
  | endSuf `isSuffixOf` loc = 0
  | otherwise = 1 + stepCounter fullInstr instrs (getNextLoc instr m loc) endSuf m

getNextLoc :: Char -> Map String (String, String) -> String -> String
getNextLoc dir m loc = if dir == 'L' then left else right
  where
    (left, right) = m ! loc

---------------------------

part2 :: String -> Map String (String, String) -> Int
part2 instr m = foldl lcm 1 steps
  where
    locs = startingNodes m
    steps = map (\loc -> stepCounter instr instr loc "Z" m) locs

startingNodes :: Map String (String, String) -> [String]
startingNodes map = filter (isSuffixOf "A") $ keys map