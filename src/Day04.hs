module Day04 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.ByteString.Char8 as BS (pack)
import Distribution.Utils.MD5 (md5)
import Control.Arrow (second)

solution = Solution "day04" "The Ideal Stocking Stuffer" run

run input = (part1 hashes, part2 hashes) where
    hashes = getHashes $ inputs input

part1 :: [(Int, String)] -> Int
part1 = findHashIdxWithLeadingZeros 5

part2 :: [(Int, String)] -> Int
part2 = findHashIdxWithLeadingZeros 6

inputs :: String -> [(Int, String)]
inputs input = map (\i -> (i, input ++ show i)) [1..]

getHashes :: [(Int, String)] -> [(Int, String)]
getHashes = map (second (show . md5 . pack))

findHashIdxWithLeadingZeros :: Int -> [(Int, String)] -> Int
findHashIdxWithLeadingZeros n = fst . head . dropWhile (any (/= '0') . take n . snd)
