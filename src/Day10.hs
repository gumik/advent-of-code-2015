module Day10 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List (iterate')

solution = Solution "day10" "Elves Look, Elves Say" run

run input = (length (elements !! 40), length (elements !! 50)) where
    firstLine = head $ lines input
    elements = iterate' lookAndSay firstLine

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay s = show (length x) ++ first : lookAndSay xs where
    (x, xs) = span (== first) s
    first = head s
