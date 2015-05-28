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
    --load menues
    generatorMenu <- mkGeneratorMenu
    iteratorMenu <- mkIteratorMenu
    cfgMenu <- mkCfgMenu
    objMenu <- mkObjMenu
    selector1 <- loadBMP "img/selector1.bmp"
    selector2 <- loadBMP "img/selector2.bmp"
    --ready to play
    run World { universe = defState
              , state = Generator
              , obj = (Objects ((Object "-" []) : o) ((length o) + 1))
              , cfg =  (Objects ((Object "-" []) : c) ((length c) + 1))
              , selected = 1
              , menues = [generatorMenu, iteratorMenu, cfgMenu, objMenu]
              , age = 0
	          , pic = [selector1, selector2] 
              }
