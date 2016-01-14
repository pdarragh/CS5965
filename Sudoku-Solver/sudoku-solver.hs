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
import System.Directory
import System.Environment
import System.Exit
import System.IO

type Board = [[Int]]
type Row = [Int]

-- print_error
--
-- Prints a supplied error message and terminates the program.
print_error :: String -> IO ()
print_error message = do
    putStrLn ("Error: " ++ message)
    exitFailure

-- print_row
--
-- Takes a row and prints it out nicely. Replaces 0 with '_'.
print_row :: Row -> String
print_row row = intercalate " " $ map show row

-- print_board
--
-- Takes a board and prints it out nicely. Replaces 0 with '_'.
print_board :: Board -> IO ()
print_board board = putStrLn $ intercalate "\n" $ map print_row board

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
            print_board $ build_board_from_lines $ filter (not . null) $ splitOn "\n" contents
        else error "Invalid filename."

-- replace_underscore_char
--
-- Returns a character given, unless that character is an underscore.
replace_underscore_char :: Char -> Char
replace_underscore_char '_' = '0'
replace_underscore_char c = c

-- replace_underscore
--
-- Replaces all instances of '_' within a string with '0' for sudoku formatting.
replace_underscore :: String -> String
replace_underscore string = map replace_underscore_char string

-- replace_zero_char
--
-- Returns a character given, unless that character is a zero.
replace_zero_char :: Char -> Char
replace_zero_char '0' = '_'
replace_zero_char c = c

-- replace_zero
--
-- Replaces all instances of '0' within a string with '_' for output formatting.
replace_zero :: String -> String
replace_zero string = map replace_zero_char string

-- read_numbers_from_line
--
-- Given a space-separated line of Sudoku numbers, returns a list of integers.
-- '_' will be converted to 0 in the list.
-- Example:
--   read_sudoku_line "1 2 3 4 5 6 7 8 9"
--     -> [1, 2, 3, 4, 5, 6, 7, 8, 9]
read_numbers_from_line :: String -> Row
read_numbers_from_line line = map (read :: String -> Int) (words $ replace_underscore line)

-- build_board_from_lines
--
-- Given a list of strings, produces a list of lists of integers.
build_board_from_lines :: [String] -> Board
build_board_from_lines lines = map read_numbers_from_line lines

-- is_valid_sudoku_line
--
-- Ensures that a given line has the appropriate number of values and that they
-- all sum to the appropriate value (45).
is_valid_sudoku_line :: Row -> Bool
is_valid_sudoku_line numbers =
    length numbers == (length (nub numbers)) &&
    length numbers == 9 &&
    all (>= 0) numbers &&
    sum numbers == 45

extract :: (Int, Int, Int) -> [Int] -> [Int]
extract (a, b, c) l = [(l !! a), (l !! b), (l !! c)]

get_square_from_lines s lines = do
    let values = ( s, s+1, s+2 )
    map (extract values) lines

-- is_valid_sudoku_board
--
-- Verifies that a board is valid.
is_valid_sudoku_board :: Board -> Bool
is_valid_sudoku_board board = do
    let loosely_valid = all is_valid_sudoku_line board
    let triples = [ (x, x+1, x+2) | x <- [0, 3, 6] ]
    True
