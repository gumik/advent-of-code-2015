module Day16 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)

solution = Solution "day16" "" run

run input = (part1 aunts, part2 aunts) where
    aunts = parse input

data AuntSue = AuntSue {
    auntId :: Int,
    auntParams :: [(String, Int)] }

parse :: String -> [AuntSue]
parse = map parseLine . lines

parseLine :: String -> AuntSue
parseLine l = AuntSue id params where
    (name, rest) = break (==':') l
    id = readNum $ splitOn " " name !! 1
    paramsRaw = splitOn ", " $ drop 2 rest
    params = map parseParam paramsRaw

parseParam :: String -> (String, Int)
parseParam s = (head parts, readNum $ parts !! 1) where
    parts = splitOn ":" s


mfscam = M.fromList [ ("children", 3)
                    , ("cats", 7)
                    , ("samoyeds", 2)
                    , ("pomeranians", 3)
                    , ("akitas", 0)
                    , ("vizslas", 0)
                    , ("goldfish", 5)
                    , ("trees", 3)
                    , ("cars", 2)
                    , ("perfumes", 1) ]

part1 :: [AuntSue] -> Int
part1 = auntId . head . filter match

match :: AuntSue -> Bool
match (AuntSue _ params) = auntValues == mfscamValues where
    keys = map fst params
    mfscamValues = map (mfscam M.!) keys
    auntValues = map snd params


part2 :: [AuntSue] -> Int
part2 = auntId . head . filter match2

match2 :: AuntSue -> Bool
match2 (AuntSue _ params) = all matchForKey params

matchForKey :: (String, Int) -> Bool
matchForKey (key, value) = (mfscam M.! key) `op` value where
    op
        | key `elem` ["cats", "trees"]            = (<)
        | key `elem` ["pomeranians", "goldfish"]  = (>)
        | otherwise                               = (==)
