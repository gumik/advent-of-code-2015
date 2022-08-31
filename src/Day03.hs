{-# LANGUAGE TupleSections #-}
module Day03 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Map.Strict as M

solution = Solution "day03" "Perfectly Spherical Houses in a Vacuum" run

run input = (part1 input, part2 input)

part1 :: String -> Int
part1 = length . visitedPositions . positions

part2 :: String -> Int
part2 input = length $ visitedPositions $ santaPositions ++ superSantaPositions where
    santaPositions = positions odds
    superSantaPositions = positions evens
    odds = map snd $ filter (odd . fst) indexed
    evens = map snd $ filter (even . fst) indexed
    indexed = [0,1..] `zip` input

type Position = (Int,Int)

positions :: [Char] -> [Position]
positions = scanl move (0,0)

move :: Position -> Char -> Position
move (x,y) direction = case direction of
    '>' -> (x+1,y)
    '<' -> (x-1,y)
    'v' -> (x,y+1)
    '^' -> (x,y-1)
    other -> error $ "Invalid character in input: '" ++ other : "'"

visitedPositions :: [Position] -> [Position]
visitedPositions = map fst . M.toList . M.fromListWith (+) . map (,1)
