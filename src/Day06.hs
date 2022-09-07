module Day06 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

solution = Solution "day06" "Probably a Fire Hazard" run

run input = (part1 instructions, part2 instructions) where
    instructions = parse input

type Point = (Int, Int)
type Rectangle = (Point, Point)
data CommandType = TurnOn | TurnOff | Toggle deriving (Show)
type Command = (CommandType, Rectangle)

parse :: String -> [Command]
parse = map parseLine . lines

parseLine :: String -> Command
parseLine s = case splitted of
    "turn":"off":rest -> (TurnOff, parseRectangle rest) 
    "turn":"on":rest  -> (TurnOn, parseRectangle rest)
    "toggle":rest     -> (Toggle, parseRectangle rest)
    _                 -> error $ "Invalid input: '" ++ s ++ "'"
    where splitted = words s

parseRectangle :: [String] -> Rectangle
parseRectangle [point1, "through", point2] = (parsePoint point1, parsePoint point2)
parseRectangle s = error $ "Invalid input: '" ++ show s ++ "'"

parsePoint :: String -> Point
parsePoint s = (x, y) where
    [x, y] = map readNum $ splitOn "," s


type Lights = M.Map Point Int
type ApplyCommand = CommandType -> Int -> Int

part1 :: [Command] -> Int
part1 = brightness applyCommand1

part2 :: [Command] -> Int
part2 = brightness applyCommand2

brightness :: ApplyCommand -> [Command] -> Int
brightness applyCommand commands = sum $ M.elems finalState where
    initialState = M.fromList [((x,y), 0) | x <- [0..999], y <- [0..999]]
    finalState = applyInstructions applyCommand initialState commands

applyInstructions :: ApplyCommand -> Lights -> [Command] -> Lights
applyInstructions applyCommand = foldl (applyInstruction applyCommand)

applyInstruction :: ApplyCommand -> Lights -> Command -> Lights
applyInstruction applyCommand lights (commandType, rectangle) = foldl (flip (M.adjust (applyCommand commandType))) lights positions where
    ((x1,y1),(x2,y2)) = rectangle
    positions = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

applyCommand1 :: ApplyCommand
applyCommand1 commandType prevState = case commandType of
    TurnOff -> 0
    TurnOn  -> 1
    Toggle  -> (prevState + 1) `mod` 2

applyCommand2 :: ApplyCommand
applyCommand2 commandType prevState = case commandType of
    TurnOff -> max 0 (prevState - 1)
    TurnOn  -> prevState + 1
    Toggle  -> prevState + 2
