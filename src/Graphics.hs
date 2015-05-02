module Graphics where

import Data.Matrix
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Game
import Init

cellSize :: Float
cellSize = 24

--Main function for drawing universe
run :: World -> IO ()
run world = case world of
            Left u -> play (InWindow "Conway`s Life" windowSize (10, 10)) 
                      white 2 world renderer handler updater
            Right g -> play (InWindow "Conway`s Life" windowSize (10, 10)) 
                       white 2 world renderer handler updater

--Calculate window size for current universe size
windowSize :: (Int, Int)
windowSize = ((round cellSize) * size * 2, (round cellSize) * size)

windowWidth :: Int
windowWidth = fst windowSize

windowHeight :: Int
windowHeight = snd windowSize

--Update the universe on each step
updater :: Float -> World -> World
updater _ (Left u) = Left (stepUniverse u)
updater _ w = w

handler :: Event -> World -> World
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) (Right g) = 
    let offsetX = fromIntegral windowWidth /2
        offsetY = fromIntegral windowHeight / 2
        i = round ((x + offsetX + cellSize / 2) / cellSize)
        j = round ((y + offsetY + cellSize / 2) / cellSize)
    in Right (setElem (inverseCell $ g ! (i, j)) (i, j) g)
handler (EventKey (SpecialKey KeyEnter) Down _ _) w = case w of
    Left u -> Right u
    Right g -> Left g
handler (EventKey (SpecialKey KeySpace) Down _ _) (Right g) = 
    Right defState
handler _ w = w

--Render a picture for each step
renderer :: World -> Picture
renderer (Left u) = let offsetX = - fromIntegral windowWidth / 2
                        offsetY = - fromIntegral windowHeight / 2
                    in translate offsetX offsetY (drawUniverse u)
renderer (Right g) = let offsetX = - fromIntegral windowWidth / 2
                         offsetY = - fromIntegral windowHeight / 2
                     in translate offsetX offsetY (drawUniverse g)

--Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                 (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                 (u ! (x, y)) 
                 | x <- [1 .. size], y <- [1 .. size]] <> drawMenu

drawMenu :: Picture
drawMenu = translate (1.5*h) (h/2) $
           pictures [color (greyN 0.7) $ rectangleSolid h h, rectangleWire h h]
               where h = fromIntegral windowHeight

--Create picture of dead (empty rectangle) or alive (solid rectangle) cell
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire
                                 Alive -> rectangleSolid
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 (figure cellSize cellSize)
