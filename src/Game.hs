module Game where

import Data.Matrix
import Graphics.Gloss

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

cellSize = 24

--Main function for running the automata
run :: Universe -> IO ()
run universe = play (InWindow "Conway`s Life" (windowSize universe) (240, 160)) 
               white 2 universe renderer handler updater

--Create pictures of dead (empty rectangle) or alive (solid rectangle) cells
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire
                                 Alive -> rectangleSolid
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 (figure cellSize cellSize)

--Calculate window size for current universe size
windowSize :: Universe -> (Int, Int)
windowSize u = ((round cellSize) * (nrows u), (round cellSize) * (ncols u))

--Check if the cell is alive
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

--Get moore neighbourhood of a cell as a list of neighbouring cells
getNeighbours :: Universe -> Int -> Int -> [Cell]
getNeighbours u x y = [u ! (i, j) | i <- [x - 1 .. x + 1], 
                                    j <- [y - 1 .. y + 1],
                                    i >= 1, i <= (ncols u),
                                    j >= 1, j <= (nrows u),
                                    i /= x || j /= y]

--Updates the cell according to the number of its neighbours
stepCellAux :: Cell -> [Cell] -> Cell
stepCellAux cell neighbours = let live = length (filter isAlive neighbours) in 
                                case cell of
                                    Dead -> if live == 3 then 
                                                Alive 
                                            else    
                                                Dead
                                    Alive -> if live == 2 || live == 3 then 
                                                Alive 
                                             else 
                                                Dead

--Update function for each cell
stepCell :: Universe -> Int -> Int -> Cell
stepCell u x y = stepCellAux (u ! (x, y)) (getNeighbours u x y)

--Auxillary function to update the universe cell by cell
stepUniverseAux :: Universe -> Universe -> Int -> Int -> Universe
stepUniverseAux oldu newu 1 1 = setElem (stepCell oldu 1 1) (1, 1) newu
stepUniverseAux oldu newu x 1 = stepUniverseAux 
                                       oldu 
                                       (setElem (stepCell oldu x 1) (x, 1) newu)
                                       (x - 1) (ncols oldu)
stepUniverseAux oldu newu x y = stepUniverseAux 
                                       oldu 
                                       (setElem (stepCell oldu x y) (x, y) newu)
                                       x (y - 1)

--Universe updater
stepUniverse :: Universe -> Universe
stepUniverse u = stepUniverseAux u u (nrows u) (ncols u)

--Update the universe on each step
updater :: Float -> Universe -> Universe
updater _ u = stepUniverse u

--Event handler function which doesn't change the universe for now
handler _ = id

--Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                           (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                           (u ! (x, y)) 
                           | x <- [1 .. (ncols u)], y <- [1 .. (nrows u)]]

--Render a picture for each step
renderer :: Universe -> Picture
renderer u = let (windowWidth, windowHeight) = windowSize u
                 offsetX = - fromIntegral windowWidth / 2
                 offsetY = - fromIntegral windowHeight / 2
             in translate offsetX offsetY (drawUniverse u)
