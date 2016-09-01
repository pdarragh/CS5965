{-
    Buy

    Handling all the purchasing of things.
-}

module Dominion.Player.Buy where

import Dominion.Cards
import Dominion.State
import Dominion.Player.Play

addTreasure :: State -> String
addTreasure state
    | (not . null) treasures = play $ head treasures
    | otherwise = error "No treasures to play"
    where treasures = getTreasureCards (hand state)

makeBuy :: State -> String
makeBuy state = case coins state of
    0 -> undefined
    1 -> undefined
    2 -> undefined
    3 -> undefined
    4 -> undefined
    5 -> undefined
    6 -> undefined
    7 -> undefined
    _ -> undefined
