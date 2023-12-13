module Day11 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, ShowString (ShowString))
import Data.Char (ord, chr)

solution = Solution "day11" run

run input = (pass1, pass2) where
    password = head $ lines input
    pass1 = nextPass password
    pass2 = nextPass pass1

nextPass :: String -> String
nextPass input = reverse $ head $ dropWhile (not . conformsPolicy) $ tail $ iterate increment input' where
    input' = reverse input

increment :: String -> String
increment n = case n of
    []     -> []
    'h':xs -> 'j' : xs
    'n':xs -> 'p' : xs
    'k':xs -> 'm' : xs
    'z':xs -> 'a' : increment xs
    x:xs   -> chr (ord x + 1) : xs

conformsPolicy :: String -> Bool
conformsPolicy password = hasIncreasingSequenceOfAtLeastThree password && hasTwoPairsOfLetters password

hasIncreasingSequenceOfAtLeastThree :: String -> Bool
hasIncreasingSequenceOfAtLeastThree s = maximum (map length (continousLetters 1 s)) >= 3

hasTwoPairsOfLetters :: String -> Bool
hasTwoPairsOfLetters s = length (filter (== 2) $ map length (continousLetters 0 s)) >= 2

continousLetters :: Int -> String -> [String]
continousLetters _ [] = []
continousLetters diff s = continousPart : continousLetters diff rest where
    toTake = 1 + length (takeWhile ((== diff) . letterDiff) (s `zip` tail s))
    continousPart = take toTake s
    rest = drop toTake s

letterDiff :: (Char, Char) -> Int
letterDiff (a, b) = ord a - ord b
