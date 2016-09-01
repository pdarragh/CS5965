{-
    Play

    Renders text for making plays.
-}

module Dominion.Player.Play where

import Data.Char (toLower)

import Dominion.Cards

play :: Card -> String
play card
    = case card of
        Treasure t -> playTreasure t
        _ -> undefined

playTreasure :: Treasure -> String
playTreasure t = "(add " ++ map toLower (show t) ++ ")"


