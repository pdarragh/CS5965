-- 
-- Sudoku Solver
--
{-
TODO:
  * add command line arguments
    * 'verify' - determines whether a completed board is valid
    * 'solve' - solve a given board
    * 'generate' - create a new board
-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Random

type Value = Int
type Board = [Maybe Value]

data Guess = Guess {
    guessIndex      :: Int,
    guessValue      :: Value,
    guessOptions    :: [Value],
    guessBoard      :: Board
} deriving (Show)

{---------------

    MAIN

---------------}

show_usage_message :: IO ()
show_usage_message = do
    progName <- getProgName
    let lines = [
            "Sudoku Solver",
            "Usage:  " ++ progName ++ " {solve | verify } puzzle-file",
            "        " ++ progName ++ " {generate} m n"
            ]
    putStrLn $ intercalate "\n" lines

verify :: [String] -> IO ()
verify [filename] = do
    (board, m, n) <- board_from_file filename
    print_board board m n
    if board_is_correct board m n then do
        putStrLn ""
        putStrLn "Board is correct!"
    else do
        putStrLn ""
        putStrLn "Invalid board."
verify [] = print_error "Accepts one argument: the name of the file to read."
verify _ = verify []

solve :: [String] -> IO ()
solve [filename] = do
    (board, m, n) <- board_from_file filename
    attempt_solution_and_print_board board m n
solve [] = print_error "Accepts one argument: the name of the file to read."
solve _ = solve []

board_from_file :: String -> IO (Board, Int, Int)
board_from_file filename = do
    file_exists <- doesFileExist filename
    if file_exists then do
        contents <- readFile filename
        let filtered_contents = filter (not . null) $ splitOn "\n" contents
        let dimensions = map (read :: String -> Int) $ words $ head $ filtered_contents
        let m = dimensions !! 0
        let n = dimensions !! 1
        let board = build_board_from_lines $ tail $ filtered_contents
        return (board, m, n)
    else error "Invalid filename."

dispatch :: [(String, [String] -> IO ())]
dispatch
    = [
        ("verify", verify) ,
        ("solve", solve)
    ]

run :: [String] -> IO ()
run [] = show_usage_message
run (command : args)
    = case lookup command dispatch of
        Just act -> act args
        Nothing  -> show_usage_message

main :: IO ()
main = run =<< getArgs

{---------------

    PRINT

---------------}

-- print_error
--
-- Prints a supplied error message and terminates the program.
print_error :: String -> IO ()
print_error message = do
    putStrLn ("Error: " ++ message)
    exitFailure

-- print_value
--
-- Prints out a Value, giving a "_" for a Nothing.
print_value :: Maybe Value -> String
print_value Nothing = "_"
print_value n = show $ fromJust n

print_row :: [Maybe Value] -> String
print_row row = intercalate " " $ map print_value row

-- print_board
--
-- Takes a board and prints it out nicely.
print_board :: Board -> Int -> Int -> IO ()
print_board board m n = putStrLn $ intercalate "\n" $ map print_row $ rows_from_board board m n

{---------------

    READ

---------------}

read_int :: String -> Maybe Value
read_int "_" = Nothing
read_int s = Just $ (read :: String -> Int) s

-- build_board_from_lines
--
-- Given a list of strings, produces a list of lists of integers.
build_board_from_lines :: [String] -> Board
build_board_from_lines lines = map read_int $ words $ intercalate " " lines

{---------------

    MAINPULATE

---------------}

value_from_maybe :: Maybe Value -> Value
value_from_maybe Nothing = 0
value_from_maybe n = fromJust n

{---------------

    MAINPULATE ROWS

---------------}

row_indices_for_index :: Int -> Int -> Int -> [Int]
row_indices_for_index m n index = [begin..end]
    where
        begin = index - (mod index (m * n))
        end   = begin + (m * n) - 1

-- row_for_index
--
-- Pulls out a complete horizontal row from a board for a given index.
row_for_index :: Board -> Int -> Int -> Int -> [Maybe Value]
row_for_index board m n index = map (board !!) $ row_indices_for_index m n index

-- rows_from_board
--
-- Returns a list of all rows in the board.
rows_from_board :: Board -> Int -> Int -> [[Maybe Value]]
rows_from_board board m n = map (row_for_index board m n) [x * dimension | x <- [0..(dimension - 1)]]
    where dimension = m * n

{---------------

    MAINPULATE COLUMNS

---------------}

col_indices_for_index :: Int -> Int -> Int -> [Int]
col_indices_for_index m n index = take (m * n) [begin, begin + (m * n) ..]
    where begin = mod index (m * n)

-- col_for_index
--
-- Pulls out a complete vertical column from a board for a given index.
col_for_index :: Board -> Int -> Int -> Int -> [Maybe Value]
col_for_index board m n index = map (board !!) $ col_indices_for_index m n index

-- cols_from_board
--
-- Returns a list of all columns in the board.
cols_from_board :: Board -> Int -> Int -> [[Maybe Value]]
cols_from_board board m n = map (col_for_index board m n) [0..(m * n - 1)]

{---------------

    MAINPULATE GROUPS

---------------}

-- first_indices_for_groups_in_col
--
-- Gives a list of the indices of the top-left elements of each group in a
-- column.
first_indices_for_groups_in_col :: Int -> Int -> Int -> [Int]
first_indices_for_groups_in_col m n col = take m $ [i * n + (col * m) | i <- [0, 0 + (m * n) ..]]


-- first_indices_for_groups
--
-- Returns a list of all the first indices for all groups on a board.
first_indices_for_groups :: Int -> Int -> [Int]
first_indices_for_groups m n = sort $ concat $ map (first_indices_for_groups_in_col m n) $ [0..(n - 1)]

-- group_row_for_index
--
-- Pulls out a group's horizontal row from a board for a given index.
group_row_for_index :: Board -> Int -> Int -> [Maybe Value]
group_row_for_index board m index = take m $ drop (index - (mod index m)) board

-- group_for_first_index
--
-- Gives an entire group as a list for a given index.
group_for_first_index :: Board -> Int -> Int -> Int -> [Maybe Value]
group_for_first_index board m n index = concat $ map (group_row_for_index board m) $ take n [index, index + (m * n) ..]

-- group_indices_for_first_index
--
-- Given a first index for a group (i.e. the top-left index within the group),
-- returns a list of all of the other inices in the group.
group_indices_for_first_index :: Int -> Int -> Int -> [Int]
group_indices_for_first_index m n index = concat $ map (take m) [[x, x+1 ..] | x <- take n [index, index + (m * n) ..]]

-- group_indices_for_index
--
-- Like `group_indices_for_first_index`, but works for any index within the
-- scope of the board.
group_indices_for_index :: Int -> Int -> Int -> [Int]
group_indices_for_index m n index = (!! 0) $ filter (elem index) $ map (group_indices_for_first_index m n) (first_indices_for_groups m n)

-- group_for_index
--
-- Returns a list of all the values within a group given a specific index.
group_for_index :: Board -> Int -> Int -> Int -> [Maybe Value]
group_for_index board m n index = map (board !!) $ group_indices_for_index m n index

-- groups_from_board
--
-- Returns a list of all the groups on a board. Groups are (n x m) in size.
groups_from_board :: Board -> Int -> Int -> [[Maybe Value]]
groups_from_board board m n = map (group_for_first_index board m n) $ first_indices_for_groups m n

{---------------

    VERIFY

---------------}

-- board_is_correct
--
-- Affirms whether an entire board solution is correct.
board_is_correct :: Board -> Int -> Int -> Bool
board_is_correct board m n = and [
        all id $ map group_is_correct $ rows_from_board board m n ,
        all id $ map group_is_correct $ cols_from_board board m n ,
        all id $ map group_is_correct $ groups_from_board board m n
    ]

-- group_is_correct
--
-- Tests whether a list of values is correct.
group_is_correct :: [Maybe Value] -> Bool
group_is_correct group = and [
        all id $ map (`elem` values) [1..(length values)] ,
        sum values == sum [1..(length values)] ,
        (length values) == (length $ nub values)
    ]
    where values = map value_from_maybe group

-- value_in_range
--
-- Checks if a Maybe Value is within a range.
value_in_range :: Maybe Value -> Int -> Bool
value_in_range Nothing _ = False
value_in_range (Just value) max = elem value [1..max]

-- incomplete_group_is_correct
--
-- Tests whether a list of values is not incorrect. Effectively just checks that
-- all values in a group are:
--   1. unique
--   2. not outside the relevant range
incomplete_group_is_correct :: [Maybe Value] -> Int -> Bool
incomplete_group_is_correct group max
    = and [
        nonblanks == nub nonblanks ,                    -- elements are unique
        all id $ map (`value_in_range` max) nonblanks   -- elements within group
    ]
    where nonblanks = filter (not . isNothing) group

-- incomplete_board_is_correct
--
-- If a board has empty spaces, this checks that none of the filled spaces are
-- necessarily wrong.
incomplete_board_is_correct :: Board -> Int -> Int -> Bool
incomplete_board_is_correct board m n = and [
        all id $ map (`incomplete_group_is_correct` (m * n)) $ rows_from_board board m n ,
        all id $ map (`incomplete_group_is_correct` (m * n)) $ cols_from_board board m n ,
        all id $ map (`incomplete_group_is_correct` (m * n)) $ groups_from_board board m n
    ]

{---------------

    SOLVE

---------------}

-- replace_nth
--
-- Replaces the nth value in a list.
replace_nth :: Int -> a -> [a] -> [a]
replace_nth n value (x:xs)
    | n == 0 = value:xs
    | otherwise = x:replace_nth (n - 1) value xs

-- replace_pair
--
-- Replaces a value on a board.
replace_pair :: Board -> (Int, Maybe Value) -> Board
replace_pair board (index, value) = replace_nth index value board

-- solidiify_values
--
-- Given a list of values, determines whether there is only one value in the
-- list and converts it to a 'Maybe Value'.
solidify_values :: [Value] -> Maybe Value
solidify_values values
    | length values == 1 = Just (values !! 0)
    | otherwise = Nothing

-- board_from_possibilities
--
-- Takes a list of lists of values and converts to a board.
board_from_possibilities :: [[Value]] -> Board
board_from_possibilities possibilities = map solidify_values possibilities

---------
-- Finding Peer Possibilities
---------

-- possibilities_for_row_from_index
--
-- Finds all the possible values for the peers of an index within a row.
possibilities_for_row_from_index :: Board -> Int -> Int -> Int -> [[Value]]
possibilities_for_row_from_index board m n index
    = map snd $ delete (index, index_possibilities) indexed_row_possibilities
    where
        indexed_row_possibilities = zip (row_indices_for_index m n index) row_possibilities
        row_possibilities   = map (possibilities_for_index board m n) (row_indices_for_index m n index)
        index_possibilities = possibilities_for_index board m n index

-- possibilities_for_col_from_index
--
-- Finds all the possible values for the peers of an index within a column.
possibilities_for_col_from_index :: Board -> Int -> Int -> Int -> [[Value]]
possibilities_for_col_from_index board m n index
    = map snd $ delete (index, index_possibilities) indexed_col_possibilities
    where
        indexed_col_possibilities = zip (col_indices_for_index m n index) col_possibilities
        col_possibilities   = map (possibilities_for_index board m n) (col_indices_for_index m n index)
        index_possibilities = possibilities_for_index board m n index

-- possibilities_for_group_from_index
--
-- Finds all the possible values for the peers of an index within a group.
possibilities_for_group_from_index :: Board -> Int -> Int -> Int -> [[Value]]
possibilities_for_group_from_index board m n index
    = map snd $ delete (index, index_possibilities) indexed_group_possibilities
    where
        indexed_group_possibilities = zip (group_indices_for_index m n index) group_possibilities
        group_possibilities = map (possibilities_for_index board m n) (group_indices_for_index m n index)
        index_possibilities = possibilities_for_index board m n index

--------
-- Obtaining A Unique Possibility From Peers
--------

-- unique_possibility_from_row
--
-- Looks at an index's peers along a row and determines whether a unique
-- solution can be found based only on those values.
unique_possibility_from_row :: Board -> Int -> Int -> Int -> Maybe Value
unique_possibility_from_row board m n index
    | length filtered_possibilities == 1 = Just $ fst (filtered_possibilities !! 0)
    | otherwise = Nothing
    where
        filtered_possibilities = filter ((== True) . snd)
            $ zip index_possibilities
            $ map null
            $ map (filtered_possibility row_possibilities) index_possibilities
        row_possibilities   = possibilities_for_row_from_index board m n index
        index_possibilities = possibilities_for_index board m n index

-- unique_possibility_from_col
--
-- Looks at an index's peers along a column and determines whether a unique
-- solution can be found based only on those values.
unique_possibility_from_col :: Board -> Int -> Int -> Int -> Maybe Value
unique_possibility_from_col board m n index
    | length filtered_possibilities == 1 = Just $ fst (filtered_possibilities !! 0)
    | otherwise = Nothing
    where
        filtered_possibilities = filter ((== True) . snd)
            $ zip index_possibilities
            $ map null
            $ map (filtered_possibility col_possibilities) index_possibilities
        col_possibilities   = possibilities_for_col_from_index board m n index
        index_possibilities = possibilities_for_index board m n index

-- unique_possibility_from_group
--
-- Looks at an index's peers within a group and determines whether a unique
-- solution can be found based only on those values.
unique_possibility_from_group :: Board -> Int -> Int -> Int -> Maybe Value
unique_possibility_from_group board m n index
    | length filtered_possibilities == 1 = Just $ fst (filtered_possibilities !! 0)
    | otherwise = Nothing
    where
        filtered_possibilities = filter ((== True) . snd)
            $ zip index_possibilities
            $ map null
            $ map (filtered_possibility group_possibilities) index_possibilities
        group_possibilities = possibilities_for_group_from_index board m n index
        index_possibilities = possibilities_for_index board m n index

--------
-- Finding An Index's Unique Possibility
--------

-- unique_possibility_for_index
--
-- If there is only one possible value for the given index, it is returned.
-- Otherwise, a Nothing is returned.
unique_possibility_for_index :: Board -> Int -> Int -> Int -> Maybe Value
unique_possibility_for_index board m n index
    | (not . null) possibilities = possibilities !! 0
    | otherwise = Nothing
    where
        possibilities = filter (not . isNothing)
            $ [unique_row_possibility] ++ [unique_col_possibility] ++ [unique_group_possibility]
        unique_row_possibility   = unique_possibility_from_row board m n index
        unique_col_possibility   = unique_possibility_from_col board m n index
        unique_group_possibility = unique_possibility_from_group board m n index

-- possibilities_for_index
--
-- Returns all possible values for a given index. This is concluded purely by
-- ruling out values from peers (e.g. this square can't be 3 because there is
-- already a 3 in the same row).
possibilities_for_index :: Board -> Int -> Int -> Int -> [Value]
possibilities_for_index board m n index
    = case (board !! index) of
        Nothing    -> filtered_possibilities_for_index board m n index
        Just value -> [value]

-- filtered_possibility
--
-- Given a list of lists containing possible values for peers and a possible
-- value for the current square, returns a list of peers' values if they also
-- have that value.
filtered_possibility :: [[Value]] -> Value -> [[Value]]
filtered_possibility group_possibilities match
    = filter (elem match) group_possibilities

-- filtered_possibilities_for_index
--
-- Finds all possible legal values for a given index, based on already-selected
-- values on the board.
filtered_possibilities_for_index :: Board -> Int -> Int -> Int -> [Value]
filtered_possibilities_for_index board m n index = possibilities
    where
        possibilities = filter_possibilities col_pos row
        col_pos       = filter_possibilities group_pos col
        group_pos     = filter_possibilities [1..(m * n)] group
        row           = row_for_index board m n index
        col           = col_for_index board m n index
        group         = group_for_index board m n index

-- filter_possibilities
--
-- Used to remove already-selected values from a list of possibilities. That is,
-- it will return [1, 2, 5, 9] from the following zone (for example):
--   _ 4 6
--   3 7 _
--   _ _ 8
--
-- possibilities: list of possible remaining integer answers
-- values: list of possible values from the board
filter_possibilities :: [Int] -> [Maybe Value] -> [Value]
filter_possibilities [] _ = []
filter_possibilities possibilities [] = possibilities
filter_possibilities possibilities values
    = case head values of
        Nothing    -> filter_possibilities possibilities (tail values)
        Just value -> filter_possibilities (delete value possibilities) (tail values)

--------
-- Solving the Board
--------

-- attempt_solution_and_print_board
--
-- Takes a board (and its dimensions) and attempts to solve the board and print
-- the result.
attempt_solution_and_print_board :: Board -> Int -> Int -> IO ()
attempt_solution_and_print_board board m n = do
    (solved, solution) <- solve_board_with_backtracking board m n []
    if solved
        then print_board solution m n
        else if (null solution)
            then attempt_solution_and_print_board board m n
            else do
                putStrLn "Could not compute solution."
                putStrLn ""
                print_board solution m n

-- solve_board_step
--
-- Searches the entire board for blank squares to which there is only one
-- possible solution and returns a board with those solutions filled in.
solve_board_step :: Board -> Int -> Int -> Board
solve_board_step board m n
    = map (unique_possibility_for_index board m n) [0..(m * n) * (m * n) - 1]

-- solve_board
--
-- Attempts to compute a solution for a sudoku board. If a solution can not be
-- found, the incomplete board is returned along with a False. If the solution
-- is found, the completed board is returned with a True.
solve_board :: Board -> Int -> Int -> (Bool, Board)
solve_board board m n
    | board_solved       = (True, board)    -- board is filled out
    | board == new_board = (False, board)   -- board is not filled but cannot proceed
    | otherwise          = solve_board new_board m n    -- recur!
    where
        board_solved = null $ filter isNothing board
        new_board    = solve_board_step board m n

-- need: a method which takes an index and a list of values and makes a random
-- selection from among the values and puts it in the specified index

-- guess_from_values
--
-- Makes a random selection from a list of values.
-- The random seed is discarded because we don't want reproducibility. Gasp!
guess_from_values :: [Value] -> IO Value
guess_from_values [] = error "No values to choose from."
guess_from_values values = do
    random_gen <- newStdGen
    let (index, _) = randomR (0, (length values) - 1) random_gen
    return $ values !! index

-- a method which takes a board and makes a random move on it, returning
-- the necessary values to track that change
-- returns: (Int, Value, [Value], Board)
-- meaning: (index, chosen value, non-chosen values, new board)
make_guess_for_board :: Board -> Int -> Int -> IO (Guess, Board)
make_guess_for_board board m n
    | null empty_indices = error "No blank spots to guess."
    | otherwise = do
        random_gen <- newStdGen
        let
            (i, g)          = randomR (0, (length empty_indices) - 1) random_gen
            index           = empty_indices !! i
            possibilities   = possibilities_for_index board m n index
        if (null possibilities)
            then make_guess_for_board board m n
            else do
                value <- guess_from_values possibilities
                let new_board = replace_pair board (index, Just value)
                let guess = Guess {
                    guessIndex      = index,
                    guessValue      = value,
                    guessOptions    = (delete value possibilities),
                    guessBoard      = board
                }
                return (guess, new_board)
    where 
        empty_indices = map fst $ filter (isNothing . snd)
                        $ zip [0..(m*n) * (m*n) - 1] board

-- board_can_be_guessed
--
-- Determines whether a board is in a position where a guess can be made.
board_can_be_guessed :: Board -> Int -> Int -> Bool
board_can_be_guessed board m n
    | null $ filter isNothing board
        -- No spots to guess.
        = False
    | otherwise
        -- Check whether any of the blank spaces have no possibilities.
        -- If any have none, then something is wrong and the board cannot proceed.
        = null $ filter null $ map (possibilities_for_index board m n) $ map fst $ filter (isNothing . snd) $ zip [0..] board

-- guess_on_board
--
-- Takes a board with blank spaces and makes a guess, returning the guess and
-- the new board state.
guess_on_board :: Board -> Int -> Int -> IO (Guess, Board)
guess_on_board board m n
    | board_can_be_guessed board m n
        = make_guess_for_board board m n
    | otherwise = do
        let guess = Guess {
            guessIndex      = 0,
            guessValue      = 0,
            guessOptions    = [],
            guessBoard      = []
        }
        return (guess, board)

-- backtrack_board
--
-- Given a board and a list of previous guesses, this attempts to rectify issues
-- caused by erroneous guesses made. It starts with the most-recent guess and
-- first tries to check if a different value would work better for the same
-- index. If not, it backtracks an additional guess. If there are no guesses to
-- backtrack, a new guess is made.
backtrack_board :: Board -> Int -> Int -> [Guess] -> IO ([Guess], Board)
backtrack_board board m n []
    -- No guesses have been made... so make a new one.
    | board_can_be_guessed board m n = do
        (guess, new_board) <- guess_on_board board m n
        if (guessIndex guess == 0)
            then backtrack_board board m n []   -- A bad guess was made.
            else return ([guess], new_board)    -- Made a good guess. Continue!
    | otherwise
        -- Something's gone wrong.
        = return ([], [])
backtrack_board board m n (guess:old_guesses)
    -- There are some previous guesses, so try to backtrack.
    | null $ guessOptions guess
        -- We've tried everything for this index; backtrack again.
        = backtrack_board board m n old_guesses
    | otherwise = do
        -- Try something else for this index.
        new_guess_value <- guess_from_values $ guessOptions guess
        let
            new_possibilities   = delete new_guess_value $ guessOptions guess
            new_board           = replace_pair (guessBoard guess) (guessIndex guess, Just new_guess_value)
            new_guess           = Guess {
                                    guessIndex      = guessIndex guess,
                                    guessValue      = new_guess_value,
                                    guessOptions    = new_possibilities,
                                    guessBoard      = board
                                }
            board_correct       = board_is_correct new_board m n
        return ((new_guess:old_guesses), new_board)

-- solve_board_with_backtracking
--
-- Takes a board and attempts to find a solution using the conventional "fill
-- all the possibilities" method. If that fails (there are blank spots left),
-- a guess is made. If the board ever ceases to be solvable, the guesses are
-- undone until they work.
solve_board_with_backtracking :: Board -> Int -> Int -> [Guess] -> IO (Bool, Board)
solve_board_with_backtracking board m n guesses
    | null $ filter isNothing board
        -- The board has been filled in.
        = if (board_is_correct board m n)
            then return (True, board)   -- it's done!
            else backtrack              -- something went wrong...
    | incomplete_board_is_correct board m n
        -- The board is not filled, but is so far correct.
        = if board == next_board
            then do
                -- Need to take a guess (no progress via iteration).
                (guess, guess_board) <- guess_on_board board m n
                if (guessIndex guess == 0)
                    then backtrack
                    else solve_board_with_backtracking guess_board m n (guess:guesses)
            else do
                -- Use the board with more squares and continue.
                solve_board_with_backtracking next_board m n guesses
    | otherwise
        -- The board is not filled nor is it correct... so let's rewind.
        = backtrack
    where
        (next_board_complete, next_board) = solve_board board m n
        backtrack = do
            (backtracked_guesses, backtracked_board) <- backtrack_board board m n guesses
            if (null backtracked_guesses && null backtracked_board)
                then
                    -- Something went wrong. Signal upwards to start from the beginning.
                    return (False, [])
                else
                    -- Proceed with the backtracked board.
                    solve_board_with_backtracking backtracked_board m n backtracked_guesses
