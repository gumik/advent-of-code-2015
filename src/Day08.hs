module Day08 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)

solution = Solution "day08" run

run input = let
    inputs = lines input
    rawLen = sum (map length inputs)
    decodedLenSum = sum $ map decodedLen inputs
    encodedLenSum = sum $ map encodedLen inputs
    part1 = rawLen - decodedLenSum
    part2 = encodedLenSum - rawLen
    in (part1, part2)

decodedLen :: String -> Int
decodedLen = r . init . tail where
    r [] = 0
    r (x:xs) = case x of
        '\\' -> case head xs of
            '\\' -> 1 + r (tail xs)
            '"' -> 1 + r (tail xs)
            'x' -> 1 + r (drop 3 xs)
        _    -> 1 + r xs

encodedLen :: String -> Int
encodedLen = (+2) . sum . map r where
    r x = case x of
        '"' -> 2
        '\\' -> 2
        _ -> 1
