{-
    Cards

    Defines the various cards available to the game engine.
-}

module Dominion.Cards where

import Data.Char (toLower, toUpper)

data Card
    = Treasure Treasure
    | Victory Victory
    | Kingdom Kingdom
    deriving (Read, Show, Eq)

data Treasure
    = Copper
    | Silver
    | Gold
    deriving (Read, Show, Eq)

data Victory
    = Estate
    | Duchy
    | Province
    deriving (Read, Show, Eq)

data Kingdom
    = Mine
    deriving (Read, Show, Eq)

readCards :: String -> [Card]
readCards cards = map readCard $ words cards

readCard :: String -> Card
readCard card
    | (not . null) treasureParse = Treasure $ (fst . head) treasureParse
    | (not . null) victoryParse  = Victory  $ (fst . head) victoryParse
    | (not . null) kingdomParse  = Kingdom  $ (fst . head) kingdomParse
    | otherwise = error "Invalid card"
    where
        treasureParse = reads adjustedCard::[(Treasure, String)]
        victoryParse  = reads adjustedCard::[(Victory, String)]
        kingdomParse  = reads adjustedCard::[(Kingdom, String)]
        adjustedCard  = toUpper (head card) : map toLower (tail card)

isTreasureCard :: Card -> Bool
isTreasureCard (Treasure _) = True
isTreasureCard _ = False

isVictoryCard :: Card -> Bool
isVictoryCard (Victory _) = True
isVictoryCard _ = False

isKingdomCard :: Card -> Bool
isKingdomCard (Kingdom _) = True
isKingdomCard _ = False

getTreasureCards :: [Card] -> [Card]
getTreasureCards = filter isTreasureCard

getVictoryCards :: [Card] -> [Card]
getVictoryCards = filter isVictoryCard

getKingdomCards :: [Card] -> [Card]
getKingdomCards = filter isKingdomCard

getTreasures :: [Card] -> [Treasure]
getTreasures cards = [x | Treasure x <- cards]

getVictories :: [Card] -> [Victory]
getVictories cards = [x | Victory x <- cards]

getKingdoms :: [Card] -> [Kingdom]
getKingdoms cards = [x | Kingdom x <- cards]

