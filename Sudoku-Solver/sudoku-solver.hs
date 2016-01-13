-- 
-- Sudoku Solver
--

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
        putStrLn "Reading from file."
