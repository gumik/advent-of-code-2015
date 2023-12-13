module Day05 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Map.Strict as M
import Data.List (singleton)

solution = Solution "day05" run

run input = (part1 splitted, part2 splitted) where
    splitted = words input

part1 = length . filter isNice

isNice :: String -> Bool
isNice s = containsAtLeastThreeVowels s && containsDuplicatedLetterWithDistance 1 s && doesNotContainsSpecialTuples s

containsAtLeastThreeVowels :: String -> Bool
containsAtLeastThreeVowels = (>= 3) . length . filter isVowel

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

containsDuplicatedLetterWithDistance :: Int -> String -> Bool
containsDuplicatedLetterWithDistance n s = any (uncurry (==)) (s `zip` drop n s)

doesNotContainsSpecialTuples :: String -> Bool
doesNotContainsSpecialTuples = not . any (`elem` ["ab", "cd", "pq", "xy"]) . stringToPairs

stringToPairs :: String -> [String]
stringToPairs s = zipWith (flip (:) . (:[])) (tail s) s


part2 = length . filter isNice2

isNice2 :: String -> Bool
isNice2 s = containsAtLeastTwoSamePairsOfLetters s && containsDuplicatedLetterWithDistance 2 s

-- Constructs dictionary which maps each pair to indices in the string. Eg.
--     "abcab"
--     [("ab", [0, 3]), ("bc", [1]), ("ca", [2])]
-- Then, checks if there is any list which min element - max element is >= 2.
-- That means the pair is at least two times in the string and is not overlapped.
containsAtLeastTwoSamePairsOfLetters :: String -> Bool
containsAtLeastTwoSamePairsOfLetters s = any (>= 2) differences where
    pairs = stringToPairs s
    indicesAsSingletonList = map singleton [0..]
    pairsIndices = M.elems $ M.fromListWith (++) (pairs `zip` indicesAsSingletonList)
    differences = map (\indices -> maximum indices - minimum indices) pairsIndices
