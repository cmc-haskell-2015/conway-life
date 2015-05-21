module Main where

import Files
import Game
import Graphics
import Graphics.Gloss
import Control.Applicative
import System.Directory

main :: IO ()
main = do
    --load database
    objFiles <- getDirectoryContents "database/objects" 
    confFiles <- getDirectoryContents "database/configs"
    let objNames = onlyFiles "objects" objFiles
    let confNames = onlyFiles "configs" confFiles
    objs <- sequence . map readFile $ objNames
    confs <- sequence . map readFile $ confNames
    o <- initObjects (shortName <$> objNames) objs
    c <- initObjects (shortName <$> confNames) confs
    --load images for menu
    start <- loadBMP "img/start.bmp"
    loadcfg <- loadBMP "img/loadcfg.bmp"
    loadobj <- loadBMP "img/loadobj.bmp"
    clear <- loadBMP "img/clear.bmp"
    save <- loadBMP "img/save.bmp"
    exit <- loadBMP "img/exit.bmp"
    select <- loadBMP "img/select.bmp"
    back <- loadBMP "img/back.bmp"
    stop <- loadBMP "img/stop.bmp"
    left <- loadBMP "img/left.bmp"
    right <- loadBMP "img/right.bmp"
    selector1 <- loadBMP "img/selector1.bmp"
    selector2 <- loadBMP "img/selector2.bmp"
    --ready to play
    run World { universe = defState
                  , state = Generator
                  , obj = (Objects ((Object "-" []) : o) ((length o) + 1))
                  , cfg =  (Objects ((Object "-" []) : c) ((length c) + 1))
                  , selected = 1
                  , pic = [start, loadcfg, loadobj, clear, save, exit, 
                        select, back, stop, left, right, selector1, selector2]
                  , age = 0 }
