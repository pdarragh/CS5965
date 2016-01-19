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

    MAIN

---------------}

main :: IO ()
main = do
    args <- getArgs
    let filename = args !! 0
    if (length args) /= 1 then do
        print_error "Accepts one argument: the name of the file to read."
    else do
        -- Now do the file reading.
        --let board = read_file filename
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
        else error "Invalid filename."

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

-- group_row_for_index
--
-- Pulls out a group's horizontal row from a board for a given index.
group_row_for_index :: Board -> Int -> Int -> [Maybe Value]
group_row_for_index board index m = take m $ drop (index - (mod index m)) board

-- group_for_index
--
-- Gives an entire group as a list for a given index.
group_for_index :: Board -> Int -> Int -> Int -> [Maybe Value]
group_for_index board index m n = concat $ map group_rows' $ take n [index, index + (m * n) ..]
    where group_rows' i = group_row_for_index board i m

{---------------

    VERIFY

---------------}
