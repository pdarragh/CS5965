-- 
-- Sudoku Solver
--
{-
TODO:
  * finish implementing is_valid_sudoku_board
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
type SolvedBoard = [Value]

{---------------

    MAIN

---------------}

show_usage_message = putStrLn "Usage information."

verify args = do
    if (length args) /= 1 then do
        print_error "Accepts one argument: the name of the file to read."
    else do
        -- Now do the file reading.
        let filename = args !! 0
        file_exists <- doesFileExist filename
        if file_exists then do
            -- It does exist! Let's load it.
            contents <- readFile filename
            let filtered_contents = filter (not . null) $ splitOn "\n" contents
            let dimensions = map (read :: String -> Int) $ words $ head $ filtered_contents
            let m = dimensions !! 0
            let n = dimensions !! 1
            let board = build_board_from_lines $ tail $ filtered_contents
            print_board board (m * n)
            if board_is_correct board m n then do
                putStrLn ""
                putStrLn "Board is correct!"
            else do
                putStrLn "Invalid board."
        else error "Invalid filename."

-- separate file reading to its own function

dispatch :: [(String, [String] -> IO ())]
dispatch = [
        ("verify", verify)
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
print_board :: Board -> Int -> IO ()
print_board board dimension = putStrLn $ intercalate "\n" $ map print_row $ rows_from_board board dimension

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

-- row_for_index
--
-- Pulls out a complete horizontal row from a board for a given index.
row_for_index :: Board -> Int -> Int -> [Maybe Value]
row_for_index board index size = take size $ drop (index - (mod index size)) board

-- rows_from_board
--
-- Returns a list of all rows in the board.
rows_from_board :: Board -> Int -> [[Maybe Value]]
rows_from_board board dimension = map row_for_index' [x * dimension | x <- [0..(dimension - 1)]]
    where row_for_index' i = row_for_index board i dimension

{---------------

    MAINPULATE COLUMNS

---------------}

-- col_for_index
--
-- Pulls out a complete vertical column from a board for a given index.
col_for_index :: Board -> Int -> Int -> [Maybe Value]
col_for_index board index size = map (board !!) [index, (index + size) .. ((size * size) - 1)]

-- cols_from_board
--
-- Returns a list of all columns in the board.
cols_from_board :: Board -> Int -> [[Maybe Value]]
cols_from_board board dimension = map col_for_index' [0..(dimension - 1)]
    where col_for_index' i = col_for_index board i dimension

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
        all id $ map group_is_correct $ rows_from_board board (m * n) ,
        all id $ map group_is_correct $ cols_from_board board (m * n) ,
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
