module Main where

import Init
import Game
import Graphics
import Control.Applicative
import System.Directory

main :: IO ()
main = do
    objFiles <- getDirectoryContents "database/objects" 
    confFiles <- getDirectoryContents "database/configs"
    objs <- sequence . map readFile . onlyFiles "objects" $ objFiles
    confs <- sequence . map readFile . onlyFiles "configs" $ confFiles
    run $ World (Right defState) (initObjects objs) (initConfigs confs)
