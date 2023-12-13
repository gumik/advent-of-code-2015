module Day09 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (nub, permutations)

solution = Solution "day09" run

run input = solve $ parse input

type Distances = M.Map (String, String) Int

parse :: String -> Distances
parse input = let
    distances = map parseLine $ lines input
    distances' = map (\((city1, city2), distance) -> ((city2, city1), distance)) distances
    in M.fromList $ distances ++ distances'

parseLine :: String -> ((String, String), Int)
parseLine line = let
    parts = splitOn " " line
    city1 = head parts
    city2 = parts !! 2
    distance = readNum $ parts !! 4
    in ((city1, city2), distance)

solve :: Distances -> (Int, Int)
solve distances = (minimum routesDistances, maximum routesDistances) where
    cities = nub $ map fst $ M.keys distances
    routes = permutations cities
    routesDistances = map (distance distances) routes

distance :: Distances -> [String] -> Int
distance distances sequence = sum $ map (distances M.!) pairs where
    pairs = sequence `zip` tail sequence
