module Day13 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (nub, permutations)

solution = Solution "day13" run

run input = (part1 happinessTable, part2 happinessTable) where
    happinessTable = parse input

type HappinessTable = M.Map (String, String) Int

parse :: String -> HappinessTable
parse = M.fromList . map parseLine . lines

parseLine :: String -> ((String, String), Int)
parseLine l = ((name1, name2), happinessValue) where
    parts = splitOn " " l
    name1 = head parts
    name2 = init $ parts !! 10
    rawValue = readNum $ parts !! 3
    happinessValue = case parts !! 2 of
        "gain" -> rawValue
        "lose" -> -rawValue

part1 :: HappinessTable -> Int
part1 ht = maximum $ map (hapiness ht) perms where
    names = nub $ map fst $ M.keys ht
    perms = map (head names :) $ permutations (tail names)

hapiness :: HappinessTable -> [String] -> Int
hapiness ht perm = sum $ s1 ++ s2 where
    s1 = zipWith (curry (ht M.!)) perm (tail perm ++ [head perm])
    s2 = zipWith (curry (ht M.!)) perm (last perm : init perm)

part2 :: HappinessTable -> Int
part2 ht = part1 ht' where
    names = nub $ map fst $ M.keys ht
    ht' = M.union ht $ M.fromList $ concat [ [(("you", name), 0), ((name, "you"), 0)] | name <- names]
