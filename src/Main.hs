module Main where

import Init
import Game
import Graphics
import Control.Applicative

main :: IO ()
main = do
    run (Right defState)
