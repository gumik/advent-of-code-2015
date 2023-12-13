module Day15 ( solution, score, possibilities ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (transpose)
import Control.Arrow ((&&&))

solution = Solution "day15" run

run input = (part1 possibleScores, part2 possibleScores) where
    ingredients = parse input
    possibleTeaspoons = possibilities 100 (length ingredients)
    possibleScores = map (score ingredients &&& calories ingredients) possibleTeaspoons

type Ingredient = [Int]

parse :: String -> [Ingredient]
parse = map parseLine . lines

parseLine :: String -> Ingredient
parseLine s = [capacity, durability, flavor, texture, calories] where
    parts = splitOn " " s
    capacity = readNum $ init $ parts !! 2
    durability = readNum $ init $ parts !! 4
    flavor = readNum $ init $ parts !! 6
    texture = readNum $ init $ parts !! 8
    calories = readNum $ parts !! 10

part1 :: [(Int, Int)] -> Int
part1 = maximum . map fst

part2 :: [(Int, Int)] -> Int
part2 = maximum . map fst . filter ((== 500) . snd)

calories :: [Ingredient] -> [Int] -> Int
calories ingredients teaspoons = sum $ zipWith (*) teaspoons $ map last ingredients

score :: [Ingredient] -> [Int] -> Int
score ingredients teaspoons = product $ map (max 0 . sum) $ transpose $ zipWith mult ingredients' teaspoons where
    ingredients' = map (take 4) ingredients  -- drop calories
    mult ingredient n = map (n*) ingredient

-- Produces all combinations of numbers of length 'n' that sum up to 'sum'.
possibilities :: Int -> Int -> [[Int]]
possibilities sum 1 = [[sum]]
possibilities sum n = concatMap extend begins where
    begins = [1 .. sum - n + 1]
    extend begin = map (begin :) (possibilities (sum - begin) (n - 1))
