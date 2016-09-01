{-
    Cards

    Defines the various cards available to the game engine.
-}

module Dominion.Cards where
import Data.Maybe

data CardType
    = Treasure
    | Victory
    | Action
    | Curse
    deriving (Show, Eq)

data CardAbility
    = AddCard
    | AddAction
    | AddCoin
    | UpgradeTreasure
    deriving (Show, Eq)

type CardCost       = Int
type VictoryPoint   = Int

data Card = Card {
    name        :: String,
    cardtype    :: CardType,
    cost        :: CardCost,
    abilities   :: [CardAbility],
    value       :: VictoryPoint
} deriving (Eq)

instance Show Card where
    show (Card n _ _ _ _) = show n

-- getCard
--
-- Returns the appropriate card for a given name. Throws an exception if no such
-- card exists.
getCard :: String -> Card
getCard cardName = fromMaybe (error "no such card") (lookup cardName cardDispatch)

cardDispatch :: [(String, Card)]
cardDispatch
    = [
          ("copper", copper)
        , ("silver", silver)
        , ("gold", gold)
        , ("estate", estate)
        , ("duchy", duchy)
        , ("province", province)
        , ("mine", mine)
    ]

----
-- Treasure
----
copper  :: Card
silver  :: Card
gold    :: Card
copper  = Card "copper"   Treasure 0 [AddCoin] 0
silver  = Card "silver"   Treasure 3 [AddCoin, AddCoin] 0
gold    = Card "gold"     Treasure 6 [AddCoin, AddCoin, AddCoin] 0

----
-- Victory
----
estate      :: Card
duchy       :: Card
province    :: Card
estate      = Card "estate"   Victory 2 [] 1
duchy       = Card "duchy"    Victory 5 [] 3
province    = Card "province" Victory 8 [] 6

----
-- Action
----
mine :: Card
mine = Card "mine" Action 5 [UpgradeTreasure] 0
