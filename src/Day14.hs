module Day14 ( solution, flyReinder, Reinder(..) ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List (transpose)
import qualified Data.Map.Strict as M
import GHC.Real (underflowError)

solution = Solution "day14" run

run input = (part1 reinders, part2 reinders) where
    reinders = parse input

type Speed = Int
type Duration = Int
type Rest = Int
data Reinder = Reinder Speed Duration Rest

parse :: String -> [Reinder]
parse = map parseLine . lines

parseLine :: String -> Reinder
parseLine s = Reinder speed duration rest where
    parts = splitOn " " s
    speed = readNum $ parts !! 3
    duration = readNum $ parts !! 6
    rest = readNum $ parts !! 13

part1 = maximum . map (flyReinder 2503)

flyReinder :: Int -> Reinder -> Int
flyReinder seconds (Reinder speed duration rest) = (speed * duration * n) + (speed * remainingTime) where
    n = seconds `div` (duration + rest)
    time = n * (duration + rest)
    remainingTime = min duration $ seconds - time


part2 = maximum . flyReinders 2503

-- OMG. This has to be refactored.

flyReinders :: Int -> [Reinder] -> [Int]
flyReinders seconds reinders = map snd $ M.toList w where
    x = map (\reinder -> map (`flyReinder` reinder) [1..seconds]) reinders
    y = transpose x
    z = map (zip [1 :: Int ..]) y
    initScores = M.fromList [(i,0) | i <- [1..length reinders]]
    w = foldl calcScore initScores z

calcScore :: M.Map Int Int -> [(Int, Int)] -> M.Map Int Int
calcScore scores distances = newScores where
    maxDist = maximum $ map snd distances
    winners = map fst $ filter ((== maxDist) . snd) distances
    newScores = foldl (flip (M.adjust (+1))) scores winners
