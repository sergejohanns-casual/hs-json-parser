import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.Read as Read

data Token = DelimToken | NumberToken | StringToken | NullToken deriving Show
data JsonValue = JsonObj (Map.Map String JsonValue) | JsonArr [JsonValue] | JsonString String | JsonInt Int | JsonDouble Double  | JsonNull deriving Show


-- Defines which characters are delimiters
isDelim :: Char -> Bool
isDelim = flip elem "{}[]:,"


-- Grab all of the string up to but excluding the next quote, and return the rest excluding the next quote in the second element
-- Deals with \ escape
skipQuoted' :: String -> String -> Maybe (String, String)
skipQuoted' acc "\\" = Nothing
skipQuoted' acc ('\\' : remaining) = skipQuoted' (next : '\\' : acc) remaining'
    where (next : remaining') = remaining
skipQuoted' acc "" = Nothing
skipQuoted' acc ('"' : remaining) = Just (reverse acc, remaining) 
skipQuoted' acc (r : remaining) = skipQuoted' (r : acc) remaining

-- Preapply accumulator
skipQuoted :: String -> Maybe (String, String)
skipQuoted = skipQuoted' ""


-- Grab all of the string compromising a number, and return the rest in the second element
takeNumber' :: String -> String -> Maybe (String, String)
takeNumber' acc "" = if length (filter (== '.') acc) <= 1
    then Just (reverse acc, "")
    else Nothing
takeNumber' acc (r : remaining)
    | Char.isDigit r || r == '.' = takeNumber' (r : acc) remaining
    | (Char.isSpace r || isDelim r) && length (filter (== '.') acc) <= 1 = Just (reverse acc, r : remaining)
    | otherwise = Nothing

-- Preapply accumulator
takeNumber :: String -> Maybe (String, String)
takeNumber = takeNumber' ""


-- Turn the raw JSON string into a list of token-value pairs
tokenize' :: [(Token, String)] -> String -> Maybe [(Token, String)]
tokenize' acc "" = Just (reverse acc)
tokenize' acc ('"' : remaining) = skipQuoted remaining >>= f
    where f (quoted, unquoted) = tokenize' ((StringToken, quoted) : acc) unquoted
tokenize' acc (r : remaining)
    | isDelim r = tokenize' ((DelimToken, [r]) : acc) remaining
    | Char.isSpace r = tokenize' acc remaining
    | Char.isDigit r = takeNumber remaining >>= f
    | r == 'n' && "ull" `List.isPrefixOf` remaining = tokenize' ((NullToken, "null") : acc) $ drop 3 remaining
    | otherwise = Nothing
        where f (number, notnumber) = tokenize' ((NumberToken, r : number) : acc) notnumber

-- Preapply accumulator
tokenize :: String -> Maybe [(Token, String)]
tokenize = tokenize' []


-- Find the portion of the token list that comes before the matching closing symbol
findMatching' :: Int -> [(Token, String)] -> String -> String -> [(Token, String)] -> Maybe ([(Token, String)], [(Token, String)])
findMatching' 0 acc _ _ remaining = Just (reverse acc, remaining)
findMatching' _ _ _ _ [] = Nothing
findMatching' level acc o c ((DelimToken, d) : remaining)
    | d == o = findMatching' (level + 1) ((DelimToken, d) : acc) o c remaining
    | d == c = findMatching' (level - 1) ((DelimToken, d) : acc) o c remaining
    | otherwise = findMatching' level ((DelimToken, d) : acc) o c remaining
findMatching' level acc o c (r : remaining) = findMatching' level (r : acc) o c remaining

-- Preapply accumulator
findMatching :: String -> String -> [(Token, String)] -> Maybe ([(Token, String)], [(Token, String)])
findMatching = findMatching' 1 []

-- Standard use
readArr = findMatching "[" "]"
readObj = findMatching "{" "}"


-- Parse a JSON array
parseArr' :: [JsonValue] -> [(Token, String)] -> Maybe [JsonValue]
parseArr' _ [] = Nothing
parseArr' acc [(DelimToken, "]")] = Just (reverse acc)
parseArr' acc [r, (DelimToken, "]")] = parse [r] >>= \value -> Just (reverse (value : acc))
parseArr' acc ((DelimToken, "[") : remaining) = readArr remaining >>= f
    -- Extract and parse the array
    where f (array, [(DelimToken, "]")]) = JsonArr <$> parseArr array >>= \value -> Just (reverse (value : acc))
          f (array, (DelimToken, ",") : notarray) = JsonArr <$> parseArr array >>= \value -> parseArr' (value : acc) notarray
          f _ = Nothing
parseArr' acc ((DelimToken, "{") : remaining) = readObj remaining >>= f
    -- Extract and parse the object
    where f (object, [(DelimToken, "]")]) = JsonObj <$> parseObj object >>= \value -> Just (reverse (value : acc))
          f (object, (DelimToken, ",") : notobject) = JsonObj <$> parseObj object >>= \value -> parseArr' (value : acc) notobject
          f _ = Nothing
parseArr' acc (r : (DelimToken, ",") : remaining) = parse [r] >>= \value -> parseArr' (value : acc) remaining
parseArr' _ _ = Nothing

-- Preapply accumulator
parseArr :: [(Token, String)] -> Maybe [JsonValue]
parseArr = parseArr' []


-- Parse a JSON object
parseObj' :: Map.Map String JsonValue -> [(Token, String)] -> Maybe (Map.Map String JsonValue)
parseObj' _ [] = Nothing
parseObj' acc [(DelimToken, "}")] = Just acc
parseObj' acc ((StringToken, key) : (DelimToken, ":") : remaining) = f remaining
    -- Match on key:value pair
    where f [v, (StringToken, "}")] = parse [v] >>= \value -> Just (Map.insert key value acc)
          f ((DelimToken, "[") : remaining) = readArr remaining >>= g
            -- Extract and parse the array
            where g (array, [(DelimToken, "}")]) = JsonArr <$> parseArr array >>= \value -> Just (Map.insert key value acc)
                  g (array, (DelimToken, ",") : notarray) = JsonArr <$> parseArr array >>= \value -> parseObj' (Map.insert key value acc) notarray
                  g _ = Nothing
          f ((DelimToken, "{") : remaining) = readObj remaining >>= g
            -- Extract and parse the object
            where g (object, [(DelimToken, "}")]) = JsonObj <$> parseObj object >>= \value -> Just (Map.insert key value acc)
                  g (object, (DelimToken, ",") : notobject) = JsonObj <$> parseObj object >>= \value -> parseObj' (Map.insert key value acc) notobject
                  g _ = Nothing
          f [r, (DelimToken, "}")] = parse [r] >>= \value -> Just (Map.insert key value acc)
          f (r : (DelimToken, ",") : remaining) = parse [r] >>= \value -> parseObj' (Map.insert key value acc) remaining
          f _ = Nothing
parseObj' _ _ = Nothing

-- Preapply accumulator
parseObj :: [(Token, String)] -> Maybe (Map.Map String JsonValue)
parseObj = parseObj' Map.empty


-- Parse a JSON value
parse :: [(Token, String)] -> Maybe JsonValue
parse [(NullToken, _)] = Just JsonNull
parse [(StringToken, s)] = Just (JsonString s)
parse [(NumberToken, n)]
    | '.' `elem` n = JsonDouble <$> Read.readMaybe n
    | otherwise = JsonInt <$> Read.readMaybe n
parse ((DelimToken, "[") : remaining) = JsonArr <$> parseArr remaining
parse ((DelimToken, "{") : remaining) = JsonObj <$> parseObj remaining
parse _ = Nothing -- Not a valid JSON value pattern


main = do
    print $ tokenize "{\"Hello there\" : null, \"How is it going?\": 10.42}"
    print $ tokenize "null" >>= parse
    print $ tokenize "12223" >>= parse
    print $ tokenize "12.223" >>= parse
    print $ tokenize "\"12.223\"" >>= parse
    print $ tokenize "[12.223, 12, \"hi\"]" >>= parse
    print $ tokenize "{\"hi\":42, \"bye\":11}" >>= parse
    print $ tokenize "{\"hello\" : [1, 2, [1, 2]], \"bye\" : {\"really bye\":[42]}}" >>= parse