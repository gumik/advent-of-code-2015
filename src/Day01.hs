module Day01 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)

solution = Solution "day01" run

run input = (part1 input, part2 input)

part1 :: String -> Int
part1 = sum . mapToInts

part2 :: String -> Int
part2 = length . takeWhile (>= 0) . scanl (+) 0 . mapToInts

mapToInts :: String -> [Int]
mapToInts = map parenToInt where
    parenToInt '(' = 1
    parenToInt ')' = -1
