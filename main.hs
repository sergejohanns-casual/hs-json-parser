import qualified Data.Map as Map

data JsonValue = JsonObj (Map.Map String JsonValue) | JsonArr [JsonValue] | JsonString String | JsonInt Int | JsonDouble Double  | JsonNull

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

main = undefined