module Day12 ( solution, parseJson ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List (uncons)
import Data.Char (isNumber)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Bifunctor (second)

solution = Solution "day12" "JSAbacusFramework.io" run

run input = (part1 input, part2 input)

-- Simply read all numbers and ignore structure.
part1 :: String -> Int
part1 = sum . map readNum .filter (/= "") . splitOn " " . map nonNumToSpace

nonNumToSpace :: Char -> Char
nonNumToSpace x = if isNum x then x else ' '

isNum :: Char -> Bool
isNum x = isNumber x || x == '-'


part2 input = let
    json = stringToJson input
    json' = filterRed json
    in sumNumbers json'

sumNumbers :: Json -> Int
sumNumbers json = case json of
    JsonList list -> sum $ map sumNumbers list
    JsonDict pairs -> sum $ map (sumNumbers . snd) pairs
    JsonString _ -> 0
    JsonValue value -> readNum value

filterRed :: Json -> Json
filterRed json = case json of
    JsonList list -> JsonList $ map filterRed list
    JsonDict pairs -> if any isPairRed pairs
                        then JsonDict []
                        else JsonDict $ map (second filterRed) pairs
    other -> other

isPairRed :: (Json, Json) -> Bool
isPairRed json = case json of
    (_, JsonString "red") -> True
    _                     -> False

-- Hand-made JSON parser. Why not?

type JsonDictKey = String
data Json = JsonList [Json]
          | JsonDict [(Json, Json)]
          | JsonString String
          | JsonValue String
          deriving (Show)

type ParseState = State String

stringToJson :: String -> Json
stringToJson = evalState parseJson

parseJson :: ParseState Json
parseJson = do
    state $ span isWhitespace
    char <- gets head
    jsonElem <- case char of
                    '[' -> parseList
                    '{' -> parseDict
                    '"' -> parseString
                    _   -> parseValue
    state $ span isWhitespace
    return jsonElem

whitespace :: String
whitespace = " \n\r\t"

isWhitespace :: Char -> Bool
isWhitespace = (`elem` whitespace)

breakCharacters :: String
breakCharacters = whitespace ++ "]},:"

isBreakCharacter :: Char -> Bool
isBreakCharacter = (`elem` breakCharacters)

parseValue :: ParseState Json
parseValue = do
    value <- state $ break isBreakCharacter
    return $ JsonValue value

parseString :: ParseState Json
parseString = do
    modify tail  -- drop "
    JsonString <$> readString

readString :: ParseState String
readString = do
    c <- readChar
    case c of
        '"' -> return ""
        _   -> do rest <- readString
                  return $ c:rest

parseDict :: ParseState Json
parseDict = do
    modify tail  -- drop {
    dictElems <- parseDictElems
    modify tail  -- drop }
    return $ JsonDict dictElems

parseDictElems :: ParseState [(Json, Json)]
parseDictElems = do
    char <- gets $ head . dropWhile isWhitespace
    case char of
        '}' -> return []
        ',' -> modify tail >> parseDictElems
        _   -> do
            dictElem <- parseDictElem
            rest <- parseDictElems
            return $ dictElem : rest

parseDictElem :: ParseState (Json, Json)
parseDictElem = do
    key <- parseJson
    modify tail  -- drop :
    value <- parseJson
    return (key, value)

parseList :: ParseState Json
parseList = do
    modify tail  -- drop [
    listElems <- parseListElems
    modify tail  -- drop ]
    return $ JsonList listElems

parseListElems :: ParseState [Json]
parseListElems = do
    char <- gets $ head . dropWhile isWhitespace
    case char of
        ']' -> return []
        ',' -> modify tail >> parseListElems
        _   -> do
            listElem <- parseJson
            rest <- parseListElems
            return $ listElem : rest

readChar :: ParseState Char
readChar = state $ fromJust . uncons
