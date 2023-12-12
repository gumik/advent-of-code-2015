module Day17 ( solution, combinations ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List (tails, sort, sortBy)
import Control.Arrow (Arrow(first))
import Data.Ord (comparing, Down (Down))
import Debug.Trace (traceShow, trace)

solution = Solution "day17" "" run

run = solve . parse

parse :: String -> [Int]
parse = sortBy (comparing Down) . map readNum . lines

solve :: [Int] -> (Int, Int)
solve nums = (length combs, length shortest) where
    combs = combinations 150 nums
    minLength = minimum $ map length combs
    shortest = filter ((== minLength) . length) combs

-- Version to count only
-- combinations :: Int -> [Int] -> Int
-- combinations n nums
--     | n < 0          = 0
--     | n == 0         = 1
--     | otherwise      = sum $ map (\(i, rest) -> combinations (n-i) rest) splits where
--         splits = [first last $ splitAt i nums | i <- [1..length nums]]

combinations :: Int -> [Int] -> [[Int]]
combinations n nums
    | n < 0          = []
    | n == 0         = [[]]
    | otherwise      = concatMap (\(i, rest) -> map (i:) (combinations (n-i) rest)) splits where
        splits = [first last $ splitAt i nums | i <- [1..length nums]]
