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
updater _ (World u Iterator x y z w) = World (stepUniverse u) Iterator x y z w
updater _ w = w

--Handle events from mouse and keyboard
handler :: Event -> World -> IO World
--Generator evets
--LMB
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
        wor@(World u Generator o c m p)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h + 150 - 30) && (y + offsetY <= h + 150 + 30) =  
            return $ World u Iterator o c 1 p
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 90 - 30) && (y + offsetY <= h + 90 + 30) =  
            return $ World u CfgMenu o c 1 p
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 30 - 30) && (y + offsetY <= h + 30 + 30) =  
            return $ World u ObjMenu o c 1 p
    | (x + offsetX >= w - 60) && (x + offsetX <= w + 60) &&
      (y + offsetY >= h - 30 - 30) && (y + offsetY <= h - 30 + 30) =  
            return $ World defState Generator o c m p
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 90 - 30) && (y + offsetY <= h - 90 + 30) =  
            saveWorld wor
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150 - 30) && (y + offsetY <= h - 150 + 30) =  
            exitSuccess
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) = return $ 
           World (setElem (inverseCell $ u ! (i, j)) (i, j) u) Generator o c m p
    | otherwise = return $  World u Generator o c m p
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)
--Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) (World u Generator o c m p)
    | m == 6 = return $ World u Generator o c 1 p
    | otherwise = return $ World u Generator o c (m + 1) p
--Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _) (World u Generator o c m p)
    | m == 1 = return $ World u Generator o c 6 p
    | otherwise = return $ World u Generator o c (m - 1) p
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    wor@(World u Generator o c m p) = case m of
    1 -> return $ World u Iterator o c 1 p
    2 -> return $ World u CfgMenu o c 1 p
    3 -> return $ World u ObjMenu o c 1 p
    4 -> return $ World defState Generator o c m p
    5 -> saveWorld wor
    6 -> exitSuccess
--ESC
handler (EventKey (SpecialKey KeyEsc) Down _ _) (World u s o c m p) =
    case s of
    Iterator -> exitSuccess
    Generator -> exitSuccess
--TODO: delete half cells
    CfgMenu -> return $ World u Generator o c 1 p
    ObjMenu -> return $ World u Generator o c 1 p
--default
handler _ w = return w

--Render a picture
renderer :: World -> Picture
renderer (World u s o c m p) = let offsetX = - fromIntegral windowWidth / 2
                                   offsetY = - fromIntegral windowHeight / 2
                                   menu = case s of
                                        Generator -> drawMenu1 m p
                                        Iterator -> drawMenu2 m p
                                        CfgMenu -> drawMenu3 m p
                                        ObjMenu -> drawMenu3 m p
                               in translate offsetX offsetY $
                                  pictures [(drawUniverse u), drawMenu, menu]

--Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                 (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                 (u ! (x, y)) 
                 | x <- [1 .. size], y <- [1 .. size]]

--Drawing menu background
drawMenu :: Picture
drawMenu = translate (1.5*h) (h/2) $
           pictures [color (greyN 0.7) $ rectangleSolid h h, rectangleWire h h]
           where h = fromIntegral windowHeight

--Drawing menu items
drawMenu1 :: Int -> [Picture] -> Picture
drawMenu1 _ pic = pictures [translate w (h + 150) $ pic !! 0,
                            translate w (h + 90) $ pic !! 1,
                            translate w (h + 30) $ pic !! 2,
                            translate w (h - 30) $ pic !! 3,
                            translate w (h - 90) $ pic !! 4,
                            translate w (h - 150) $ pic !! 5]
                   where w = 1.5 * (fromIntegral windowHeight)
                         h = (fromIntegral windowHeight) / 2

drawMenu2 :: Int -> [Picture] -> Picture
drawMenu2 _ pic = pictures [translate w (h + 30) $ pic !! 8,
                            translate w (h - 30) $ pic !! 5]
                   where w = 1.5 * (fromIntegral windowHeight)
                         h = (fromIntegral windowHeight) / 2

drawMenu3 :: Int -> [Picture] -> Picture
drawMenu3 _ pic = pictures [translate w (h - 30) $ pic !! 6,
                            translate w (h - 90) $ pic !! 7,
                            translate w (h - 150) $ pic !! 5]
                   where w = 1.5 * (fromIntegral windowHeight)
                         h = (fromIntegral windowHeight) / 2

--Create picture of dead (empty rectangle) or alive (solid rectangle) cell
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire
                                 Alive -> rectangleSolid
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 (figure cellSize cellSize)
