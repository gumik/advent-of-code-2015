module Day07 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Debug.Trace (traceShow)
import Data.Bits ((.&.), (.|.), complement, shift)
import qualified Data.Map.Strict as M
import Data.Word (Word16)
import Control.Monad.State

solution = Solution "day07" run

run input = let
    circuit = parse input
    circuitMap = circuitToMapByOutput circuit
    part1 = evalState (getValue "a") circuitMap
    circuitMap' = M.adjust (const $ Connection $ Number part1) "b" circuitMap
    part2 = evalState (getValue "a") circuitMap'
    in (part1, part2)

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


type CircuitMap = M.Map Output Input

circuitToMapByOutput :: Circuit -> CircuitMap
circuitToMapByOutput = M.fromList . map (\(Wiring input output) -> (output, input))

getValue :: Identifier -> State CircuitMap Word16
getValue output = do
    input <- gets (M.! output)
    value <- case input of
        Connection source -> getSourceValue source
        And source1 source2 -> do
            val1 <- getSourceValue source1
            val2 <- getSourceValue source2
            return $ val1 .&. val2
        Or source1 source2 -> do
            val1 <- getSourceValue source1
            val2 <- getSourceValue source2
            return $ val1 .|. val2
        Shift source n -> do
            val <- getSourceValue source
            return $ shift val n
        Negation source -> do
            val <- getSourceValue source
            return $ complement val
    modify $ M.adjust (const $ Connection $ Number value) output
    return value

getSourceValue :: Source -> State CircuitMap Word16
getSourceValue source = case source of
    Direct identifier -> getValue identifier
    Number n -> return n
