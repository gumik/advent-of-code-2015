module Day19 ( solution, parseMolecule, possibleReplacements ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Control.Monad.State (State(..), evalState, gets, modify)
import Data.List.Split (splitOn)
import Data.List.Extra (replace)
import Data.List (nub)
import Data.Char (isLower)
import Data.Maybe (Maybe(..), isJust, fromJust, catMaybes)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)
import Distribution.Compat.Prelude (traceShow)

solution = Solution "day19" "" run

run input = (part1 parsedInput, part2 parsedInput) where
    parsedInput = parse input

data Input = Input Replacements Molecule
type Replacements = [(String, Molecule)]
type Molecule = [String]

parse :: String -> Input
parse input = Input (parseReplacements $ head parts) (parseMolecule $ head $ lines $ last parts) where
    parts = splitOn "\n\n" input

parseReplacements :: String -> Replacements
parseReplacements = map parseLine . lines where
    parseLine s = (head parts, parseMolecule $ last parts) where
        parts = splitOn " => " s

parseMolecule :: String -> Molecule
parseMolecule s = case s of
    []                -> []
    (c1:c2:rest)
        | isLower c2  -> [c1, c2] : parseMolecule rest
        | otherwise   -> [c1] : parseMolecule (c2:rest)
    (c1:rest)         -> [c1] : parseMolecule rest

part1 :: Input -> Int
part1 (Input replacements molecule) = length $ possibilities replacements molecule

possibilities :: Replacements -> Molecule -> [Molecule]
possibilities replacements molecule = nub $ concatMap (possibilitiesPerReplacement molecule) replacements

possibilitiesPerReplacement :: Molecule -> (String, [String]) -> [Molecule]
possibilitiesPerReplacement molecule (from, to) = calc molecule [] where
    calc :: Molecule -> Molecule -> [Molecule]
    calc molecule prefix = case molecule of
        []          -> []
        (x:rest) -> if x == from
            then (prefix ++ to ++ rest) : calc rest (prefix ++ [x])
            else calc rest (prefix ++ [x])


-- Simply try to replace what is possible.
-- eg. for molecule HHOH
--     and rules: H -> HH, OH -> HH, e -> H
-- try first rule, then second, and so on
-- but "backwards", eg. HHOH becames HOH using first rule.
-- When "e" is reached, it is a solution.

part2 :: Input -> Int
part2 (Input replacements molecule) = fromJust $ shortestPath molecule where
    shortestPath :: Molecule -> Maybe Int
    shortestPath ["e"] = Just 0
    shortestPath molecule =
        let replacedList = concatMap (possibleReplacements molecule) replacements
            subResults = map shortestPath replacedList
            subResults' = catMaybes subResults
            result = case subResults' of
                [] -> Nothing
                _  -> Just $ 1 + head subResults'  -- Just find the first solution. Seems it is the only one
        in result

possibleReplacements :: Molecule -> (String, Molecule) -> [Molecule]
possibleReplacements [] _ = []
possibleReplacements molecule (to, from)
    | prefix == from  = (to : rest) : others
    | otherwise       = others
  where
    (prefix, rest) = splitAt (length from) molecule
    others = map (head molecule : ) $ possibleReplacements (tail molecule) (to, from)
