module Day21 ( solution, roundsToWin, isWin, Player(..), possibilities, possibilities' ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Extra (splitOn)

solution = Solution "day21" run

run input = (part1, part2 boss) where
    boss = parse input

data Item = Item {
    itemCost :: Int,
    itemDamage :: Int,
    itemArmor :: Int
}
weapons = [ Item  8 4 0  -- Dagger
          , Item 10 5 0  -- Shortsword
          , Item 25 6 0  -- Warhammer
          , Item 40 7 0  -- Longsword
          , Item 74 8 0  -- Greataxe
          ]

armors = [ Item  13 0 1  -- Leather
         , Item  31 0 2  -- Chainmail
         , Item  53 0 3  -- Splintmail
         , Item  75 0 4  -- Bandedmail
         , Item 102 0 5  -- Platemail
         ]

rings = [ Item  25 1 0  -- Damage +1
        , Item  50 2 0  -- Damage +2
        , Item 100 3 0  -- Damage +3
        , Item  20 0 1  -- Defense +1
        , Item  40 0 2  -- Defense +2
        , Item  80 0 3  -- Defense +3
        ]

data Player = Player {
    playerHitPoints :: Int,
    playerDamage :: Int,
    playerArmor :: Int
}

initialHP = 100

-- It was pretty easy to do it mentally.
-- Bought items:
-- Longsword    40     7       0
-- Damage +1    25     1       0
-- Leather      13     0       1
-- SUM:         78     8       1
part1 :: Int
part1 = 78

parse :: String -> Player
parse input = Player (parseValue $ head parts) (parseValue $ parts !! 1) (parseValue $ parts !! 2) where
    parts = lines input
    parseValue = readNum . last . splitOn ": "

part2 :: Player -> Int
part2 boss = maximum $ map snd $ filter (\(player, cost) -> not $ isWin player boss) possiblePlayers where
    possiblePlayers = map makePlayer possibleItems
    possibleItems = map concat $ possibilities' [possibleWeapons, possibleArmors, possibleRings]
    possibleWeapons = possibilities weapons 1        -- exactly one weapon
    possibleArmors = possibilities armors 1 ++ [[]]  -- at most one armor
    possibleRings = possibilities rings 2 ++ possibilities rings 1 ++ [[]]  -- 0..2 rings


isWin :: Player -> Player -> Bool
isWin player boss = roundsPlayer <= roundsBoss where
    roundsPlayer = roundsToWin player boss
    roundsBoss = roundsToWin boss player

roundsToWin :: Player -> Player -> Int
roundsToWin player boss = rounds + min rest 1 where
    (rounds, rest) = playerHitPoints boss `divMod` damage
    damage = max (playerDamage player - playerArmor boss) 1

-- All possible sub-lists of given length with non-repeating elements.
-- Eg. for [1,2,3] gives [[1,2], [1,3], [2,3]]
possibilities :: [a] -> Int -> [[a]]
possibilities x 1 = map (:[]) x
possibilities [] _ = []
possibilities (item:rest) n = map (item:) (possibilities rest (n-1)) ++ possibilities rest n

-- All possible combinations of elements from given groups of elements.
-- Eg. for [[1,2], [3,4], [5,6]] gives [[1,3,5], [1,3,6], [1,4,5], [1,4,6], [2,3,5], [2,3,6], [2,4,5], [2,4,6]]
possibilities' :: [[a]] -> [[a]]
possibilities' [] = [[]]
possibilities' (group:rest) = concatMap (\elem -> map (elem:) (possibilities' rest)) group

makePlayer :: [Item] -> (Player, Int)
makePlayer items = (Player initialHP damage armor, cost) where
    damage = sum $ map itemDamage items
    armor = sum $ map itemArmor items
    cost = sum $ map itemCost items
