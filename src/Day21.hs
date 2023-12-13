module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)

solution = Solution "day21" "" run

run _ = (part1, NoSolution)

-- It was pretty easy to do it mentally.
-- Bought items:
-- Longsword    40     7       0
-- Damage +1    25     1       0
-- Leather      13     0       1
-- SUM:         78     8       1
part1 :: Int
part1 = 78
