module Day02 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (sort)

solution = Solution "day02" "I Was Told There Would Be No Math" run

run input = (part1 $ parse input, part2 $ parse input)

parse :: String -> [[Int]]
parse = map (map readNum . splitOn "x") . lines

part1 :: [[Int]] -> Int
part1 = sum . map areaNeeded

areaNeeded :: [Int] -> Int
areaNeeded [x, y, z] = minimum sides + sum (map (*2) sides) where
    sides = [x*y, x*z, y*z]

part2 :: [[Int]] -> Int
part2 = sum . map ribbonNeeded

ribbonNeeded :: [Int] -> Int
ribbonNeeded lengths = 2*shortest1 + 2*shortest2 + product lengths   where
    shortest1:shortest2:_ = sort lengths
