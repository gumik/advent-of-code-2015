module Day20 ( solution, presents, divisors ) where

import Common (Solution(Solution), NoSolution(..), readNum)

solution = Solution "day20" "" run

run input = (part1 n, part2 n) where
    n = readNum input

part1 :: Int -> Int
part1 n = 1 + length (takeWhile (< n) $ map presents [1..])

divisors :: Int -> [Int]
divisors n = concatMap (\(x1, (x2, _)) -> if x1 == x2 then [x1] else [x1, x2]) properDivisors where
    limit = floor $ sqrt (fromIntegral n)
    divides = map (\x -> (x, n `divMod` x)) [1..limit]
    properDivisors = filter ((== 0) . snd . snd) divides

presents :: Int -> Int
presents n = 10 * sum (divisors n)


part2 :: Int -> Int
part2 n = 1 + length (takeWhile (< n) $ map presents' [1..])

presents' :: Int -> Int
presents' n = 11 * sum divisors' where
    divisors' = filter (noMoreThan50 n) (divisors n)

noMoreThan50 :: Int -> Int -> Bool
noMoreThan50 house x = (fromIntegral house / fromIntegral x) <= 50.0
