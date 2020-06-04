import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

data Token = DelimToken | NumberToken | StringToken | NullToken deriving Show
data JsonValue = JsonObj (Map.Map String JsonValue) | JsonArr [JsonValue] | JsonString String | JsonInt Int | JsonDouble Double  | JsonNull

isDelim :: Char -> Bool
isDelim = flip elem "{}[]:,"

-- Grab all of the string up to and including the next quote, and return the rest in the second element
-- Deals with \ escape
skipQuoted' :: String -> String -> Maybe (String, String)
skipQuoted' acc "\\" = Nothing
skipQuoted' acc ('\\':remaining) = skipQuoted' (next:'\\':acc) remaining'
    where (next:remaining') = remaining
skipQuoted' acc "" = Nothing
skipQuoted' acc ('"':remaining) = Just (reverse ('"':acc), remaining) 
skipQuoted' acc (r:remaining) = skipQuoted' (r:acc) remaining

-- Preapply accumulator
skipQuoted :: String -> Maybe (String, String)
skipQuoted = skipQuoted' ""


-- Grab all of the string compromising a number, and return the rest in the second element
takeNumber' :: String -> String -> Maybe (String, String)
takeNumber' acc "" = if length (filter (== '.') acc) <= 1
    then Just (reverse acc, "")
    else Nothing
takeNumber' acc (r:remaining)
    | Char.isDigit r || r == '.' = takeNumber' (r:acc) remaining
    | (Char.isSpace r || isDelim r) && length (filter (== '.') acc) <= 1 = Just (reverse acc, r:remaining)
    | otherwise = Nothing

-- Preapply accumulator
takeNumber :: String -> Maybe (String, String)
takeNumber = takeNumber' ""


-- Turn the raw JSON string into a list of distinct tokens
tokenize' :: [(Token, String)] -> String -> Maybe [(Token, String)]
tokenize' acc "" = Just (reverse acc)
tokenize' acc ('"':remaining) = skipQuoted remaining >>= f
    where f (quoted, unquoted) = tokenize' ((StringToken, '"':quoted):acc) unquoted
tokenize' acc (r:remaining)
    | isDelim r = tokenize' ((DelimToken, [r]):acc) remaining
    | Char.isSpace r = tokenize' acc remaining
    | Char.isDigit r = takeNumber remaining >>= f
    | r == 'n' && "ull" `List.isPrefixOf` remaining = tokenize' ((NullToken, "null"):acc) $ drop 3 remaining
    | otherwise = Nothing
        where f (number, notnumber) = tokenize' ((NumberToken, r:number):acc) notnumber

-- Preapply accumulator
tokenize :: String -> Maybe [(Token, String)]
tokenize = tokenize' []


-- Parse a JSON value

{-
-- Grab all of the string up to but exluding the closing brace, and return the rest in the second element.
-- Deals with \ escape
untilMatchingBrace' :: Int -> String -> String -> Maybe (String, String)
untilMatchingBrace' 0 acc remains = Just ((reverse . tail) acc, remains) -- Slice of the closing brace
untilMatchingBrace' level acc "" = Nothing
untilMatchingBrace' level acc ('\\':remains) = untilMatchingBrace' level (next:'\\':acc) remains'
    where (next:remains') = remains
untilMatchingBrace' level acc ('{':remains) = untilMatchingBrace' (level + 1) ('{':acc) remains
untilMatchingBrace' level acc ('}':remains) = untilMatchingBrace' (level - 1) ('}':acc) remains
untilMatchingBrace' level acc ('"':remains) = skipQuoted remains >>= f
    where f (quoted, unquoted) = untilMatchingBrace' level (reverse quoted ++ ('"':acc)) unquoted
untilMatchingBrace' level acc (r:remains) = untilMatchingBrace' level (r:acc) remains

-- Preapply level and accumulator
untilMatchingBrace :: String -> Maybe (String, String)
untilMatchingBrace = untilMatchingBrace' 1 ""

splitKVPairs :: String -> [(String, String)]
splitKVPairs = undefined

parse :: String -> Either String JsonValue
parse = undefined
-}

main = do
    print $ tokenize "{\"Hello there\":null, \"How is it going?\": 10.42}"