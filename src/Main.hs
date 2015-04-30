module Main where

import Init
import Game
import Graphics
import Control.Applicative

main :: IO ()
main = do
    putStrLn "Enter field width:"
    w <- getInt
    putStrLn "Enter field height:"
    h <- getInt
    run (Right (defState w h))

getInt :: IO Int
getInt = read <$> getLine
