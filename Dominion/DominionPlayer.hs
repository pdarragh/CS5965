{-
    Dominion Player

    A rudimentary Dominion player.

    Like *really* rudimentary.
-}

-- import Dominion.Cards
import Dominion.State
import Dominion.Player

main :: IO ()
main = getInput

getInput :: IO ()
getInput = do
    input <- getLine
    parseInput input
    getInput

move :: String -> IO ()
move state = putStrLn $ takeTurn $ buildState state

parseInput :: String -> IO ()
parseInput s
    | firstWord == "move" = move $ unwords $ drop 1 splitWords
    | otherwise = return () -- just do nothing
    where
        splitWords  = words $ drop 1 $ reverse $ drop 1 $ reverse s
        firstWord   = head splitWords
