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
    let objNames = onlyFiles "objects" objFiles
    let confNames = onlyFiles "configs" confFiles
    objs <- sequence . map readFile $ objNames
    confs <- sequence . map readFile $ confNames
    let o = zip (map shortName objNames) objs
    let c = zip (map shortName confNames) confs
    print o
    print c
    run $ World (Right defState) (initObjects o) (initConfigs c)
