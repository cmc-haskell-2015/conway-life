module Main where

import Control.Exception
import Init
import Game
import Graphics

main :: IO ()
main = do 
    putStrLn "Configuration file:"
    config <- getLine
    result <- try $ readFile config :: IO (Either IOException String)
    case result of
      Left exception -> putStrLn $ "Error: " ++ show exception
      Right content -> run . initUni . positions $ content
