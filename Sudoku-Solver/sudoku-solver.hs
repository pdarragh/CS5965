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

type Value = Int
type Board = [Maybe Value]

{---------------

    MAIN

---------------}

show_usage_message :: IO ()
show_usage_message = putStrLn "Usage information."

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
row_for_index board m n index = take dimension $ drop (index - (mod index dimension)) board
    where dimension = m * n

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
col_for_index board m n index = map (board !!) [index, (index + dimension) .. ((dimension * dimension) - 1)]
    where dimension = m * n

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
    | board_solved       = (True, board)
    | board == new_board = (False, board)
    | otherwise          = solve_board new_board m n
    where
        board_solved = null $ filter (== True) $ map isNothing board
        new_board    = solve_board_step board m n

attempt_solution_and_print_board :: Board -> Int -> Int -> IO ()
attempt_solution_and_print_board board m n
    | solved = print_board solution m n
    | otherwise = do
        putStrLn "Could not compute solution."
        putStrLn ""
        print_board solution m n
    where
        (solved, solution) = solve_board board m n
