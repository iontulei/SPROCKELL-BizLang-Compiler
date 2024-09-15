module Main where

import MyParser
import MyElaborator
import MyCodeGen
import Sprockell

import System.Environment


-- compiles a file into a spril program and displays the output
compile :: FilePath -> IO ()
compile filePath = do
    input <- readFile filePath
    let output = createAST input
    case output of
        -- error
        Left  err -> do
            print err

        -- compile ast
        Right ast -> do
            let env = compileProgram ast
            let threads = mainCode env : threadsCode env
            -- putStrLn $ "Main Code: " ++ show (mainCode env)
            -- putStrLn $ "Threads code: " ++ show (threadsCode env)
            run threads


-- takes the first argument (file path) and passes it to the compile function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> compile filePath
        []         -> error "Please give a file path something!"