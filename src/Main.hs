module Main where

import Files
import Game
import Graphics
import Graphics.Gloss
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
    start <- loadBMP "img/start.bmp"
    loadcfg <- loadBMP "img/loadcfg.bmp"
    loadobj <- loadBMP "img/loadobj.bmp"
    clear <- loadBMP "img/clear.bmp"
    save <- loadBMP "img/save.bmp"
    exit <- loadBMP "img/exit.bmp"
    select <- loadBMP "img/select.bmp"
    back <- loadBMP "img/back.bmp"
    stop <- loadBMP "img/stop.bmp"
    run $ World defState Generator (Objects o (length o)) (Configs c (length c))
            1 [start, loadcfg, loadobj, clear, save, exit, select, back, stop]
