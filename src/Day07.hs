module Day07 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Debug.Trace (traceShow)
import Data.Bits ((.&.), (.|.), complement, shift)
import qualified Data.Map.Strict as M
import Data.Word (Word16)

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
            | Number Word16
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
        "LSHIFT" -> Shift s1 (readNum id2)
        "RSHIFT" -> Shift s1 (-readNum id2)
        where
            [id1, op, id2] = splitOn " " s
            s1 = parseSource id1
            s2 = parseSource id2

parseSource :: String -> Source
parseSource s
    | all isDigit s = Number $ fromIntegral (readNum s)
    | otherwise     = Direct s

part1 circuit = let
    circuitMap = circuitToMapByOutput circuit
    in getValue circuitMap "a"

type CircuitMap = M.Map Output Input

circuitToMapByOutput :: Circuit -> CircuitMap
circuitToMapByOutput = M.fromList . map (\(Wiring input output) -> (output, input))

getValue :: CircuitMap -> Identifier -> Word16
getValue circuitMap output = let
    input = circuitMap M.! output in case input of
        Connection source -> getSourceValue circuitMap source
        And source1 source2 -> getSourceValue circuitMap source1 .&. getSourceValue circuitMap source2
        Or source1 source2 -> getSourceValue circuitMap source1 .|. getSourceValue circuitMap source2
        Shift source n -> shift (getSourceValue circuitMap source) n
        Negation source -> complement (getSourceValue circuitMap source)

getSourceValue :: CircuitMap -> Source -> Word16
getSourceValue circuitMap source = case source of
    Direct identifier -> getValue circuitMap identifier
    Number n -> n
