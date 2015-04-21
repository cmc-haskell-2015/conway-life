module Graphics where

import Data.Matrix
import Graphics.Gloss
import Game

--Main function for drawing universe
run :: Universe -> IO ()
run universe = play (InWindow "Conway`s Life" (windowSize universe) (240, 160)) 
               white 2 universe renderer handler updater

--Calculate window size for current universe size
windowSize :: Universe -> (Int, Int)
windowSize u = ((round cellSize) * (nrows u), (round cellSize) * (ncols u))

--Update the universe on each step
updater :: Float -> Universe -> Universe
updater _ u = stepUniverse u

--Event handler, useless now but helpful in bonus part
handler _ = id

--Render a picture for each step
renderer :: Universe -> Picture
renderer u = let (windowWidth, windowHeight) = windowSize u
                 offsetX = - fromIntegral windowWidth / 2
                 offsetY = - fromIntegral windowHeight / 2
             in translate offsetX offsetY (drawUniverse u)

--Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                           (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                           (u ! (x, y)) 
                           | x <- [1 .. (ncols u)], y <- [1 .. (nrows u)]]

--Create picture of dead (empty rectangle) or alive (solid rectangle) cell
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire
                                 Alive -> rectangleSolid
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 (figure cellSize cellSize)
