module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, inArrayBounds)
import qualified Data.Map.Strict as M
import Data.Array (Array, assocs, (//), (!), elems, bounds)

solution = Solution "day18" run

run input = (part1 lights, part2 lights) where
    lights = parse input

data Light = On | Off deriving (Eq)
type Lights = Array (Int, Int) Light

instance Show Light where
    show On  = "#"
    show Off = "."

parse :: String -> Lights
parse = parseArray readChar where
    readChar x = case x of
        '#' -> On
        _   -> Off

part1 :: Lights -> Int
part1 = turnedOn . (!! 100) . iterate animate

part2 = turnedOn . addCorners . (!! 100) . iterate (animate . addCorners) where
    addCorners l = l // [ ((y, x), On) | y <- [y0, ym], x <- [x0, xm] ] where
        ((y0, x0), (ym, xm)) = bounds l

turnedOn :: Lights -> Int
turnedOn = length . filter (== On) . elems

animate :: Lights -> Lights
animate lights = lights // map animateOne (assocs lights) where

    animateOne :: ((Int, Int), Light) -> ((Int, Int), Light)
    animateOne ((y, x), light) = ((y, x), light') where
        light' = case light of
            On -> if neighboursOn == 2 || neighboursOn == 3 then On else Off
            Off -> if neighboursOn == 3 then On else Off
        neighboursOn = length $ filter (== On) $ neighbours (y, x)

    neighbours :: (Int, Int) -> [Light]
    neighbours (y, x) = [lights ! (y', x') | y' <- [y-1..y+1], x' <- [x-1..x+1],
                                             (y', x') /= (y, x),
                                             inArrayBounds lights (y', x')]
