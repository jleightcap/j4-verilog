module Main where

import Data.Map as M
import System.Environment (getArgs)

import Compile
import Parse
import Scope
import Types

main :: IO ()
main = do
    args <- getArgs
    txt <- getContents
    ast <- parseIO txt
    if "-p" `elem` args
        then print ast
        else do
            env <- scope ast defaultEnv
            let env' = deadCodeElim ast env
            putStrLn (compile env' ast)
