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
updater _ world@(World u Iterator _ _ _ _ a) = world { universe = stepUniverse u
                                                     , age = a + 1 }
updater _ w = w

--Handle events from mouse and keyboard
handler :: Event -> World -> IO World
--Generator events
--LMB
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
    world@(World u Generator _ _ _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h + 150 - 30) && (y + offsetY <= h + 150 + 30) =  
            return $ world { state = Iterator
                           , selected = 1 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 90 - 30) && (y + offsetY <= h + 90 + 30) =  
            return $ world { state = CfgMenu 1
                           , selected = 1 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 30 - 30) && (y + offsetY <= h + 30 + 30) =  
            return $ world { state = ObjMenu 1 Nothing
                           , selected = 1 }
    | (x + offsetX >= w - 60) && (x + offsetX <= w + 60) &&
      (y + offsetY >= h - 30 - 30) && (y + offsetY <= h - 30 + 30) =  
            return $ world { universe = defState } 
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 90 - 30) && (y + offsetY <= h - 90 + 30) =  
            saveWorld world
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150 - 30) && (y + offsetY <= h - 150 + 30) =  
            exitSuccess
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) = return $ world {
           universe = (setElem (inverseCell $ u ! (i, j)) (i, j) u) }
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)
--Mouse move
handler (EventMotion (x, y)) world@(World _ Generator _ _ _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h + 150 - 30) && (y + offsetY <= h + 150 + 30) =  
            return $ world { selected = 1 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 90 - 30) && (y + offsetY <= h + 90 + 30) =  
            return $ world { selected = 2 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 30 - 30) && (y + offsetY <= h + 30 + 30) =  
            return $ world { selected = 3 }
    | (x + offsetX >= w - 60) && (x + offsetX <= w + 60) &&
      (y + offsetY >= h - 30 - 30) && (y + offsetY <= h - 30 + 30) =  
            return $ world { selected = 4 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 90 - 30) && (y + offsetY <= h - 90 + 30) =  
            return $ world { selected = 5 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150 - 30) && (y + offsetY <= h - 150 + 30) =  
            return $ world { selected = 6 }
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
--Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) 
    world@(World _ Generator _ _ m _ _)
    | m == 6 = return $ world { selected = 1 }
    | otherwise = return $ world { selected = m + 1 }
--Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _)
    world@(World _ Generator _ _ m _ _)
    | m == 1 = return $ world { selected = 6 }
    | otherwise = return $ world { selected = m - 1 }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u Generator o c m p a) = case m of
    1 -> return $ World u Iterator o c 1 p a
    2 -> return $ World u (CfgMenu 1) o c 1 p a
    3 -> return $ World u (ObjMenu 1 Nothing) o c 1 p a
    4 -> return $ World defState Generator o c m p a
    5 -> saveWorld world
    6 -> exitSuccess
--Iterator Events
--INSANE!
--LMB
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
    world@(World u Iterator _ _ _ _ _)
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h) && (y + offsetY <= h + 60) =  
            return $ world { state = Generator
                           , selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 60) && (y + offsetY <= h) =  
            exitSuccess
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
--Mouse move
handler (EventMotion (x, y)) world@(World _ Iterator _ _ _ _ _)
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h) && (y + offsetY <= h + 60) =  
            return $ world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 60) && (y + offsetY <= h) =  
            return $ world { selected = 2 }
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
--Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) 
    world@(World _ Iterator _ _ m _ _)
    | m == 2 = return $ world { selected = 1 }
    | otherwise = return $ world { selected = m + 1 }
--Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _)
    world@(World _ Iterator _ _ m _ _)
    | m == 1 = return $ world { selected = 2 }
    | otherwise = return $ world { selected = m - 1 }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u Iterator o c m p a) = case m of
    1 -> return $ World u Generator o c 1 p a
    2 -> exitSuccess
--CfgMenu Events
--INSANE!!
--LMB
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
    world@(World u (CfgMenu n)  _ c _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            return $ world { universe = halfToAlive $ loadConfig $ 
                                        (cList c) !! (n - 1)
                           , state = Generator }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            return $ world { state = Generator
                           , selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 210) && (y + offsetY <= h - 150) =  
            exitSuccess
    | (x + offsetX >= w - 200 - 16) && (x + offsetX <= w - 200 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) =
        if n == 1 then return $ world { state = CfgMenu (cNum c) }
                  else return $ world { state = CfgMenu (n - 1) }
    | (x + offsetX >= w + 200 - 16) && (x + offsetX <= w + 200 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) =
        if n == (cNum c) then return $ world { state = CfgMenu 1 }
                         else return $ world { state = CfgMenu (n + 1) }
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
--Mouse move
handler (EventMotion (x, y)) world@(World _ (CfgMenu n) _ _ _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            return $ world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            return $ world { selected = 2 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 210) && (y + offsetY <= h - 150) =  
            return $ world { selected = 3 }
    | otherwise = return $ world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
--Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) 
    world@(World _ (CfgMenu n) _ _ m _ _)
    | m == 3 = return $ world { selected = 1 }
    | otherwise = return $ world { selected = m + 1 }
--Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _)
    world@(World _ (CfgMenu n) _ _ m _ _)
    | m == 1 = return $ world { selected = 3 }
    | otherwise = return $ world { selected = m - 1 }
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) 
    world@(World _ (CfgMenu n) _ c _ _ _)
    | n == (cNum c) = return $ world { state = CfgMenu 1 }
    | otherwise = return $ world { state = CfgMenu (n + 1) }
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) 
    world@(World _ (CfgMenu n) _ c _ _ _)
    | n == 1 = return $ world { state = CfgMenu (cNum c) }
    | otherwise = return $ world { state = CfgMenu (n - 1) }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u (CfgMenu n) o c m p a) = case m of
    1 -> return $ world { universe = halfToAlive $ loadConfig $ 
                                        (cList c) !! (n - 1)
                        , state = Generator }
    2 -> return $ World u Generator o c 1 p a
    3 -> exitSuccess
--ObjMenu Events
--IM BECOMING INSANE!!!
--LMB
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
    world@(World u (ObjMenu n c) o _ _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            return $ world { state = Generator
                           , selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            exitSuccess
    | (x + offsetX >= w - 120 - 16) && (x + offsetX <= w - 120 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) =
        if n == 1 then return $ world { state = ObjMenu (oNum o) c }
                  else return $ world { state = ObjMenu (n - 1) c }
    | (x + offsetX >= w + 120 - 16) && (x + offsetX <= w + 120 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) =
        if n == (oNum o) then return $ world { state = ObjMenu 1 c }
                         else return $ world { state = ObjMenu (n + 1) c }
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) = return $ world {
           universe = halfToAlive $ loadObject u ((oList o) !! (n - 1)) 
                                    (Just (i, j)) }
    | otherwise = return world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)
--Mouse move
handler (EventMotion (x, y)) world@(World _ (ObjMenu n c) _ _ _ _ _)
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            return $ world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            return $ world { selected = 2 }
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) = return $ world {
           state = ObjMenu n (Just (i, j)) }
    | otherwise = return $ world { state = ObjMenu n Nothing }
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)
--Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) 
    world@(World _ (ObjMenu n c) _ _ m _ _)
    | m == 2 = return $ world { selected = 1 }
    | otherwise = return $ world { selected = m + 1 }
--Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _)
    world@(World _ (ObjMenu n c) _ _ m _ _)
    | m == 1 = return $ world { selected = 2 }
    | otherwise = return $ world { selected = m - 1 }
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) 
    world@(World _ (ObjMenu n c) o _ _ _ _)
    | n == (oNum o) = return $ world { state = ObjMenu 1 c }
    | otherwise = return $ world { state = ObjMenu (n + 1) c }
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) 
    world@(World _ (ObjMenu n c) o _ _ _ _)
    | n == 1 = return $ world { state = ObjMenu (oNum o) c }
    | otherwise = return $ world { state = ObjMenu (n - 1) c }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u (ObjMenu _ _) o c m p a) = case m of
    1 -> return $ World u Generator o c 1 p a
    2 -> exitSuccess
--ESC
handler (EventKey (SpecialKey KeyEsc) Down _ _) world = exitSuccess
--default
handler _ w = return w

--Render a picture
renderer :: World -> Picture
renderer (World u s o c m p a) = let offsetX = - fromIntegral windowWidth / 2
                                     offsetY = - fromIntegral windowHeight / 2
                                     menu = case s of
                                        Generator -> drawMenu1 m p
                                        Iterator -> drawMenu2 m p
                                        CfgMenu n -> drawMenu3 m p n c
                                        ObjMenu n _-> drawMenu4 m p n o
                                     uni = case s of
                                        Generator -> u
                                        Iterator -> u
                                        CfgMenu n -> loadConfig $ 
                                            (cList c) !! (n - 1)
                                        ObjMenu n coords -> loadObject u 
                                            ((oList o) !! (n - 1)) coords
                                  in translate offsetX offsetY $
                                     pictures [(drawUniverse uni), 
                                               (drawMenu a), menu]

--Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                 (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                 (u ! (x, y)) 
                 | x <- [1 .. size], y <- [1 .. size]]

--Drawing menu background
drawMenu :: Integer -> Picture
drawMenu age = translate (1.5*h) (h/2) $ pictures [color (greyN 0.7) $ 
                    rectangleSolid h h, rectangleWire h h,
                    translate (-100) (h/2 - 40) $ Scale 0.3 0.3 $ 
                    Text $ "Iteration: " ++  (show age)]
               where h = fromIntegral windowHeight

--Drawing menu items
drawMenu1 :: Int -> [Picture] -> Picture
drawMenu1 m pic = let j = case m of
                        1 -> h + 150
                        2 -> h + 90
                        3 -> h + 30
                        4 -> h - 30
                        5 -> h - 90
                        6 -> h - 150
                      i = case m of
                        1 -> 65
                        2 -> 125
                        3 -> 125
                        4 -> 60
                        5 -> 50
                        6 -> 50
                  in pictures [translate w (h + 150) $ pic !! 0,
                               translate w (h + 90) $ pic !! 1,
                               translate w (h + 30) $ pic !! 2,
                               translate w (h - 30) $ pic !! 3,
                               translate w (h - 90) $ pic !! 4,
                               translate w (h - 150) $ pic !! 5,
                               translate (w - 30 - i) j $ pic !! 11,
                               translate (w + 30 + i) j $ pic !! 12]
                   where w = 1.5 * (fromIntegral windowHeight)
                         h = (fromIntegral windowHeight) / 2

drawMenu2 :: Int -> [Picture] -> Picture
drawMenu2 m pic = let j = if m == 1 then h + 30 else h - 30
                      i = 50
                  in pictures [translate w (h + 30) $ pic !! 8,
                               translate w (h - 30) $ pic !! 5,
                               translate (w - 30 - i) j $ pic !! 11,
                               translate (w + 30 + i) j $ pic !! 12]
                  where w = 1.5 * (fromIntegral windowHeight)
                        h = (fromIntegral windowHeight) / 2

drawMenu3 :: Int -> [Picture] -> Int -> Configs -> Picture
drawMenu3 m pic n c = let j = case m of
                                1 -> h - 60
                                2 -> h - 120
                                3 -> h - 180
                          i = if m == 1 then 65 else 50
                      in pictures [translate w (h - 60) $ pic !! 6,
                                   translate w (h - 120) $ pic !! 7,
                                   translate w (h - 180) $ pic !! 5,
                                   translate (w - 200) (h + 10) $ pic !! 9,
                                   translate (w + 200) (h + 10) $ pic !! 10,
                                   translate (w - 30 - i) j $ pic !! 11,
                                   translate (w + 30 + i) j $ pic !! 12,
                                   translate (w - 75) (h + 50) $ Scale 0.2 0.2 $
                                    Text $ "Config " ++ show n ++ "/" ++ 
                                                        show (cNum c),
                                   translate (w - 80) h $ Scale 0.1 0.1 $
                                    Text $ name $ (cList c) !! (n - 1)]
                      where w = 1.5 * (fromIntegral windowHeight)
                            h = (fromIntegral windowHeight) / 2

drawMenu4 :: Int -> [Picture] -> Int -> Objects -> Picture
drawMenu4 m pic n o = let j = if m == 1 then h - 60 else h - 120
                          i = 50
                      in pictures [translate w (h - 60) $ pic !! 7,
                                   translate w (h - 120) $ pic !! 5,
                                   translate (w - 120) (h + 10) $ pic !! 9,
                                   translate (w + 120) (h + 10) $ pic !! 10,
                                   translate (w - 30 - i) j $ pic !! 11,
                                   translate (w + 30 + i) j $ pic !! 12,
                                   translate (w - 75) (h + 50) $ Scale 0.2 0.2 $
                                    Text $ "Object " ++ show n ++ "/" ++ 
                                                        show (oNum o),
                                   translate (w - 40) h $ Scale 0.2 0.2 $
                                    Text $ name $ (oList o) !! (n - 1)]
                      where w = 1.5 * (fromIntegral windowHeight)
                            h = (fromIntegral windowHeight) / 2

--Create picture of dead (empty rectangle) or alive (solid rectangle) cell
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire c c
                                 Alive -> rectangleSolid c c
                                 Half -> color (greyN 0.4) $ rectangleSolid c c
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 figure
                    where c = cellSize
