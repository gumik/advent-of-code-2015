module Day07 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Debug.Trace (traceShow)

solution = Solution "day07" "Some Assembly Required" run

run input = (part1 circuit, NoSolution)
    where circuit = parse input

type Circuit = [Wiring]
type Identifier = String
data Wiring = Wiring Input Output deriving Show
type Output = Identifier
data Input = And Source Source
           | Or Source Source
           | Shift Source Int
           | Negation Source
           | Connection Source
           deriving Show
data Source = Direct Identifier
            | Number Int
            deriving Show

parse :: String -> Circuit
parse = map parseLine . lines

parseLine :: String -> Wiring
parseLine line = let
    [p1, p2] = splitOn " -> " line
    output = p2
    input = parseInput p1
    in Wiring input output

parseInput :: String -> Input
parseInput s
    | ' ' `notElem` s     = Connection $ parseSource s
    | take 3 s == "NOT"   = Negation $ Direct $ drop 4 s
    | otherwise           = case op of
        "AND" -> And s1 s2
        "OR"  -> Or s1 s2
        "LSHIFT" -> Shift s1 (-readNum id2)
        "RSHIFT" -> Shift s1 (readNum id2)
        where
            [id1, op, id2] = splitOn " " s
            s1 = parseSource id1
            s2 = parseSource id2

parseSource :: String -> Source
parseSource s
    | all isDigit s = Number $ readNum s
    | otherwise     = Direct s

part1 circuit = traceShow circuit NoSolution