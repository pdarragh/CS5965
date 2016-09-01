{-
    State

    Provides the ability to manipulate the State string of a Dominion game.
-}

module Dominion.State where

import Dominion.Cards
import Dominion.Substrings
import Data.List (elemIndex)
import Data.Maybe

data State = State {
    players     :: [String],
    supply      :: [Card],
    trash       :: [Card],
    actions     :: Int,
    buys        :: Int,
    coins       :: Int,
    deck        :: [Card],
    hand        :: [Card],
    plays       :: [Card],
    discards    :: [Card]
} deriving (Show, Eq)

-- buildState
--
-- Constructs an entire State data object out of a properly-formatted State
-- input string.
buildState :: String -> State
buildState state = State _players _supply _trash _actions _buys _coins _deck _hand _plays _discards
    where
        _players    = getPlayers state
        _supply     = map readCard $ getSupply state
        _trash      = map readCard $ getTrash state
        _actions    = getActions state
        _buys       = getBuys state
        _coins      = getCoins state
        _deck       = map readCard $ getDeck state
        _hand       = map readCard $ getHand state
        _plays      = map readCard $ getPlays state
        _discards   = map readCard $ getDiscards state

-- getFromState
--
-- Takes a State and finds an inner portion according to the value being sought.
-- i.e.
--   getFromState "(move ((players one two) (supply copper copper silver)))" "players"
--   -> Just ["one", "two"]
getFromState :: String -> String -> [String]
getFromState valueName state
    | isNothing index = []
    | otherwise = case rparen of
        Nothing -> []
        Just x  -> words $ slice 0 (x - 1) cutoff
    where
        index = findSubstringIn state valueName
        cutoff = drop (fromJust index + length valueName) state
        rparen = elemIndex ')' cutoff

-- getIntFromState
--
-- Takes a State and finds an inner portion according to the value being sought,
-- and converts the first of those values to an Int.
-- i.e.
--   getIntFromState "(move ((players one two) (actions 4)))" "actions"
--   -> 4
getIntFromState :: String -> String -> Int
getIntFromState valueName state
    | null fromState = 0
    | otherwise = read (head fromState) :: Int
    where fromState = getFromState valueName state

----
-- Values to Lists of Strings
----

getPlayers  :: String -> [String]
getSupply   :: String -> [String]
getTrash    :: String -> [String]
getDeck     :: String -> [String]
getHand     :: String -> [String]
getPlays    :: String -> [String]
getDiscards :: String -> [String]

getPlayers  = getFromState "players"
getSupply   = getFromState "supply"
getTrash    = getFromState "trash"
getDeck     = getFromState "deck"
getHand     = getFromState "hand"
getPlays    = getFromState "plays"
getDiscards = getFromState "discards"

----
-- Values to Ints
----

getActions  :: String -> Int
getBuys     :: String -> Int
getCoins    :: String -> Int

getActions  = getIntFromState "actions"
getBuys     = getIntFromState "buys"
getCoins    = getIntFromState "coins"
