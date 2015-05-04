module Graphics where

import Data.Matrix
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import Files
import System.Exit

cellSize :: Float
cellSize = 24

--Main function for drawing universe
run :: World -> IO ()
run world = playIO (InWindow "Conway`s Life" windowSize (10, 10)) 
                       white 2 world 
                       (return . renderer) 
                       handler 
                       (\x y -> return $ updater x y)

--Calculate window size for current universe size
windowSize :: (Int, Int)
windowSize = ((round cellSize) * size * 2, (round cellSize) * size)

windowWidth :: Int
windowWidth = fst windowSize

windowHeight :: Int
windowHeight = snd windowSize

--Update the universe on each step
updater :: Float -> World -> World
updater _ (World (Left u) x y) = World (Left (stepUniverse u)) x y
updater _ w = w

--Handle events from mouse and keyboard
{- TODO: left - switch to objects menu
         right - switch to config menu
         up/down - scroll current menu
-}
handler :: Event -> World -> IO World
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
        (World (Right g) o c) = 
    let offsetX = fromIntegral windowWidth / 2
        offsetY = fromIntegral windowHeight / 2
        i = round ((x + offsetX + cellSize / 2) / cellSize)
        j = round ((y + offsetY + cellSize / 2) / cellSize)
    in return $
        if (i <= size) && (j <= size) then 
            World (Right (setElem (inverseCell $ g ! (i, j)) (i, j) g)) o c
        else World (Right g) o c
handler (EventKey (SpecialKey KeyEnter) Down _ _) (World w o c) = return $
    case w of
    Left u -> World (Right u) o c
    Right g -> World (Left g) o c
handler (EventKey (SpecialKey KeySpace) Down _ _) (World (Right g) o c) = 
    return $ World (Right defState) o c
handler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess 
handler (EventKey (SpecialKey KeyF2) Down _ _) w = saveWorld w
handler _ w = return w

--Render a picture for each step
-- TODO: draw panels
renderer :: World -> Picture
renderer (World (Left u) o c) = let offsetX = - fromIntegral windowWidth / 2
                                    offsetY = - fromIntegral windowHeight / 2
                                in translate offsetX offsetY (drawUniverse u)
renderer (World (Right g) o c) = let offsetX = - fromIntegral windowWidth / 2
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
