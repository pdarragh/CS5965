{-
    Substrings

    For basic substring manipulations.
-}

module Dominion.Substrings where

import Data.List (elemIndex)
import Data.Maybe

-- slice
--
-- Similar to a Python slice of an array. Returns an empty version of the array
-- if the slice does not exist (i.e. negative slices).
--
-- Taken from http://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- findSubstringIn
--
-- Given a string and a substring, finds the index of the first occurrence of
-- that substring within the string. Returns a Nothing if the substring cannot
-- be found.
findSubstringIn :: String -> String -> Maybe Int
findSubstringIn s sub
    | isNothing index = Nothing
    | slice (fromJust index) (length sub - 1) s == sub = index
    | otherwise = case result of
        Nothing -> Nothing
        Just x  -> Just (x + 1)
    where
        index  = elemIndex (head sub) s
        result = findSubstringIn (drop 1 s) sub

{-
-- hasBalancedParens
--
-- Ensures parentheses are in order and balanced within a string.
hasBalancedParens :: String -> Bool
hasBalancedParens s = (== 0) $ parenCount s 0

parenCount :: String -> Int -> Int
parenCount  ""        result = result
parenCount ('(':rest) result = parenCount rest (result + 1)
parenCount (')':rest) result = parenCount rest (result - 1)
parenCount ( _ :rest) result = parenCount rest result

data WordList = Word String | Words [WordList] deriving Show

parseInput :: String -> WordList
parseInput "" = Words []
parseInput ('(':s) =
    if lpi < rpi then
        -- need to recurse
        Words $ map Word $ words $ take (fromJust lpi) s
    else
        -- return
        Words $ map Word $ words $ take (fromJust rpi) s
    where
        lpi = elemIndex '(' s
        rpi = elemIndex ')' s
-}
