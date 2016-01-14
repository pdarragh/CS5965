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

import Data.List
import Data.List.Split
import System.Directory
import System.Environment
import System.Exit
import System.IO

-- print_error
--
-- Prints a supplied error message and terminates the program.
print_error :: String -> IO ()
print_error message = do
    putStrLn ("Error: " ++ message)
    exitFailure

main = do
    args <- getArgs
    if (length args) /= 1 then do
        print_error "Accepts one argument: the name of the file to read."
    else do
        -- Now do the file reading.
        let filename = args !! 0
        read_file filename

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

-- read_numbers_from_line
--
-- Given a space-separated line of Sudoku numbers, returns a list of integers.
-- '_' will be converted to 0 in the list.
-- Example:
--   read_sudoku_line "1 2 3 4 5 6 7 8 9"
--     -> [1, 2, 3, 4, 5, 6, 7, 8, 9]
read_numbers_from_line :: String -> [Int]
read_numbers_from_line line = map (read :: String -> Int) (words $ replace_underscore line)

-- build_board_from_lines
--
-- Given a list of strings, produces a list of lists of integers.
build_board_from_lines :: [String] -> [[Int]]
build_board_from_lines lines = map read_numbers_from_line lines

-- is_valid_sudoku_line
--
-- Ensures that a given line has the appropriate number of values and that they
-- all sum to the appropriate value (45).
is_valid_sudoku_line :: [Int] -> Bool
is_valid_sudoku_line numbers =
    length numbers == (length (nub numbers)) &&
    length numbers == 9 &&
    all (>= 0) numbers &&
    sum numbers == 45

-- is_valid_sudoku_board
--
-- Verifies that a board is valid.
is_valid_sudoku_board :: [[Int]] -> Bool
is_valid_sudoku_board board = all is_valid_sudoku_line board

-- read_file
--
-- Given a filename (as a string), attempts to read a sudoku board from the
-- supplied file. Verifies that the file exists.
read_file :: String -> IO ()
read_file filename = do
    -- Verify the file exists.
    file_exists <- doesFileExist filename
    if not file_exists then do
        -- It does not. Quit.
        print_error ("File does not exist: '" ++ filename ++ "'")
    else do
        -- It does exist! Let's load it.
        contents <- readFile filename
        let board = build_board_from_lines $ filter (not . null) $ splitOn "\n" contents
        if is_valid_sudoku_board board then do
            putStrLn "Valid"
        else do
            putStrLn "Invalid"
