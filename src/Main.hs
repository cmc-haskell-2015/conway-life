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
    o <- initObjects (shortName <$> objNames) objs
    c <- initObjects (shortName <$> confNames) confs
    run $ World (Right defState) (Objects o (length o)) (Configs c (length c))
