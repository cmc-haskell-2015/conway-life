module Game where

import Data.Matrix
import Graphics.Gloss

data Cell = Dead | Alive deriving (Show)

cellSize = 24

type Universe = (Matrix Cell)

drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire
                                 Alive -> rectangleSolid
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 (figure cellSize cellSize)

run :: Universe -> IO ()
run universe = play (InWindow "Conway`s Life" (windowSize universe) (240, 160)) 
               white 2 universe renderer handler updater

windowSize :: Universe -> (Int, Int)
windowSize u = ((round cellSize) * (nrows u), (round cellSize) * (ncols u))

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

getNeighbours :: Universe -> Int -> Int -> [Cell]
getNeighbours u x y = [u ! (i, j) | i <- [x - 1 .. x + 1], 
                                    j <- [y - 1 .. y + 1],
                                    i >= 1, i <= (ncols u),
                                    j >= 1, j <= (nrows u),
                                    i /= x || j /= y]

stepCellAux :: Cell -> [Cell] -> Cell
stepCellAux cell neighbours = let live = length (filter isAlive neighbours)
                           in case cell of
                               Dead -> if live == 3 then Alive else Dead
                               Alive -> if live == 2 || live == 3 then Alive else Dead

stepCell :: Universe -> Int -> Int -> Cell
stepCell u x y = stepCellAux (u ! (x, y)) (getNeighbours u x y)

stepUniverseAux :: Universe -> Universe -> Int -> Int -> Universe
stepUniverseAux oldu newu 1 1 = setElem (stepCell oldu 1 1) (1, 1) newu
stepUniverseAux oldu newu x 1 = stepUniverseAux oldu 
                                                (setElem (stepCell oldu x 1) (x, 1) newu)
                                                (x - 1) (ncols oldu)
stepUniverseAux oldu newu x y = stepUniverseAux oldu 
                                                (setElem (stepCell oldu x y) (x, y) newu)
                                                x (y - 1)

stepUniverse :: Universe -> Universe
stepUniverse u = stepUniverseAux u u (nrows u) (ncols u)

updater :: Float -> Universe -> Universe
updater _ u = stepUniverse u

--handler :: Event -> Universe -> Universe
handler _ = id

drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
             (u ! (x, y)) | x <- [1 .. (ncols u)], y <- [1 .. (nrows u)]]

renderer :: Universe -> Picture
renderer u = let (windowWidth, windowHeight) = windowSize u
                 offsetX = - fromIntegral windowWidth / 2
                 offsetY = - fromIntegral windowHeight / 2
             in translate offsetX offsetY (drawUniverse u)
