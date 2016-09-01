{-
    Player

    Handles all of the abilities of a player.
-}

module Dominion.Player where

import Dominion.Cards
import Dominion.State
import Dominion.Player.Action
import Dominion.Player.Buy
import Dominion.Player.CleanUp

takeTurn :: State -> String
takeTurn state
    | shouldTakeAction  = takeAction state
    | shouldAddTreasure = addTreasure state
    | buys state > 0    = makeBuy state
    | otherwise         = doCleanUp state
    where
        shouldTakeAction = actions state > 0 && (not . null) (getKingdomCards $ hand state)
        shouldAddTreasure = (not . null) (getTreasureCards $ hand state)
