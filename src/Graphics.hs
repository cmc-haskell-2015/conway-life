-- | Graphics interface of the programm
module Graphics where

import Data.Matrix
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import Files
import System.Exit

-- | Used in handling mouse events
data MyMouseEvent
    = Move -- ^ when mouse moves
    | Click -- ^ when pressing left mouse button

-- | 'Convert' MyMouseEvent to Bool
isClick :: MyMouseEvent -> Bool
isClick Click = True
isClick Move = False

-- | Getter for picture width
getWidth :: Picture -> Float
getWidth (Bitmap w _ _ _) = fromIntegral w
getWidth _ = 0

-- | Getter for picture height
getHeight :: Picture -> Float
getHeight (Bitmap _ h _ _) = fromIntegral h
getHeight _ = 0

-- | Cell size
cellSize :: Float
cellSize = 24

-- | Main function for drawing universe
run :: World -> IO ()
run world = playIO (InWindow "Conway`s Life" windowSize (10, 10)) 
                       white 2 world 
                       (return . renderer) 
                       handler 
                       (\x y -> return $ updater x y)

-- | Calculate window size for current universe size
windowSize :: (Int, Int)
windowSize = ((round cellSize) * size * 2, (round cellSize) * size)

-- | Window width
windowWidth :: Int
windowWidth = fst windowSize

-- | Window height
windowHeight :: Int
windowHeight = snd windowSize

-- | Update the universe on each step
updater :: Float -> World -> World
updater _ world@(World u Iterator _ _ _ _ a) = world { universe = stepUniverse u
                                                     , age = a + 1 }
updater _ w = w

-- | Handle mouse events in main menu (Generator state)
generatorMouse :: MyMouseEvent -> Float -> Float -> World -> IO World
generatorMouse e x y world
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h + 150 - 30) && (y + offsetY <= h + 150 + 30) =  
            case e of
                Click -> return world { state = Iterator, selected = 1 }
                Move -> return world { selected = 1 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 90 - 30) && (y + offsetY <= h + 90 + 30) =  
            case e of
                Click -> return world { state = CfgMenu 1, selected = 1 }
                Move -> return world { selected = 2 }
    | (x + offsetX >= w - 125) && (x + offsetX <= w + 125) &&
      (y + offsetY >= h + 30 - 30) && (y + offsetY <= h + 30 + 30) =  
            case e of
                Click -> return world { state = ObjMenu 1 Nothing, selected = 1}
                Move -> return world { selected = 3 }
    | (x + offsetX >= w - 60) && (x + offsetX <= w + 60) &&
      (y + offsetY >= h - 30 - 30) && (y + offsetY <= h - 30 + 30) =  
            case e of
                Click -> return world { universe = defState } 
                Move -> return world { selected = 4 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 90 - 30) && (y + offsetY <= h - 90 + 30) =  
            case e of
                Click -> saveWorld world
                Move -> return world { selected = 5 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150 - 30) && (y + offsetY <= h - 150 + 30) =  
            case e of
                Click -> exitSuccess
                Move -> return world { selected = 6 }
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) && (isClick e) = let 
            u = universe world in return world { universe = 
                    (setElem (inverseCell $ u ! (i, j)) (i, j) u) }
    | otherwise = return world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)

-- | Handle mouse events during simulation (Iterator state)
iteratorMouse :: MyMouseEvent -> Float -> Float -> World -> IO World
iteratorMouse e x y world
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h) && (y + offsetY <= h + 60) =  
            case e of
                Click -> return world { state = Generator, selected = 1 }
                Move -> return world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 60) && (y + offsetY <= h) =  
            case e of
                Click -> exitSuccess
                Move -> return world { selected = 2 }
    | otherwise = return world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2

-- | Handle mouse events in menu for loading configs (CfgMenu state)
cfgMenuMouse :: MyMouseEvent -> Float -> Float -> World -> IO World
cfgMenuMouse e x y world
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            case e of
                Click -> return world { universe = halfToAlive $ loadConfig $ 
                                        (list c) !! (n - 1), state = Generator }
                Move -> return world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            case e of
                Click -> return world { state = Generator, selected = 1 }
                Move -> return world { selected = 2 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 210) && (y + offsetY <= h - 150) =  
            case e of
                Click -> exitSuccess
                Move -> return world { selected = 3 }
    | (x + offsetX >= w - 200 - 16) && (x + offsetX <= w - 200 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) && 
      (isClick e) = if n == 1 then return world { state = CfgMenu (num c) }
                              else return world { state = CfgMenu (n - 1) }
    | (x + offsetX >= w + 200 - 16) && (x + offsetX <= w + 200 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) &&
      (isClick e) = if n == (num c) then return world { state = CfgMenu 1 }
                                    else return world { state = CfgMenu (n + 1)}
    | otherwise = return world
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          c = cfg world
          n = cnum $ state world

-- | Handle mouse events in menu for loading objects (ObjMenu state)
objMenuMouse :: MyMouseEvent -> Float -> Float -> World -> IO World
objMenuMouse e x y world
    | (x + offsetX >= w - 65) && (x + offsetX <= w + 65) &&
      (y + offsetY >= h - 90) && (y + offsetY <= h - 30) =  
            case e of
                Click -> return world { state = Generator, selected = 1 }
                Move -> return world { selected = 1 }
    | (x + offsetX >= w - 50) && (x + offsetX <= w + 50) &&
      (y + offsetY >= h - 150) && (y + offsetY <= h - 90) =  
            case e of
                Click -> exitSuccess
                Move -> return world { selected = 2 }
    | (x + offsetX >= w - 120 - 16) && (x + offsetX <= w - 120 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) &&
      (isClick e) = if n == 1 then return world { state = ObjMenu (num o) c }
                              else return world { state = ObjMenu (n - 1) c }
    | (x + offsetX >= w + 120 - 16) && (x + offsetX <= w + 120 + 16) &&
      (y + offsetY >= h + 10 - 16) && (y + offsetY <= h + 10 + 16) &&
      (isClick e) = if n == (num o) then return world { state = ObjMenu 1 c }
                                else return world { state = ObjMenu (n + 1) c }
    | (isClick e) = return world { universe = halfToAlive $ 
            loadObject (universe world) ((list o) !! (n - 1)) (Just (i, j)) }
    | otherwise = return world { state = ObjMenu n (Just (i, j)) }
    where offsetX = fromIntegral windowWidth / 2
          offsetY = fromIntegral windowHeight / 2
          w = 1.5 * (fromIntegral windowHeight)
          h = (fromIntegral windowHeight) / 2
          i = round ((x + offsetX + cellSize / 2) / cellSize)
          j = round ((y + offsetY + cellSize / 2) / cellSize)
          o = obj world
          n = onum $ state world
          c = crd $ state world

-- | Function for menu navigation by the up & down keys
-- Int parameter is step and should be 1 or -1
keyMenuNavigation :: World -> Int -> World
keyMenuNavigation world step
    | m + step < 1 = world { selected = n }
    | m + step > n = world { selected = 1 }
    | otherwise = world { selected = m + step }
    where m = selected world
          n = case (state world) of
                Generator -> 6
                Iterator -> 2
                CfgMenu _ -> 3
                ObjMenu _ _ -> 2

-- | Handle events from mouse and keyboard
handler :: Event -> World -> IO World
-- Mouse Events
-- Click
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
    world@(World _ s _ _ _ _ _) = case s of
        Generator -> generatorMouse Click x y world
        Iterator -> iteratorMouse Click x y world
        CfgMenu _ -> cfgMenuMouse Click x y world
        ObjMenu _ _ -> objMenuMouse Click x y world
-- Move
handler (EventMotion (x, y)) 
    world@(World _ s _ _ _ _ _) = case s of
        Generator -> generatorMouse Move x y world
        Iterator -> iteratorMouse Move x y world
        CfgMenu _ -> cfgMenuMouse Move x y world
        ObjMenu _ _ -> objMenuMouse Move x y world
-- Keyboard Events
-- Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) world = 
    return $ keyMenuNavigation world 1
-- Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _) world = 
    return $ keyMenuNavigation world (-1)
-- Generator events
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
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u Iterator o c m p a) = case m of
    1 -> return $ World u Generator o c 1 p a
    2 -> exitSuccess
--CfgMenu Events
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) 
    world@(World _ (CfgMenu n) _ c _ _ _)
    | n == (num c) = return $ world { state = CfgMenu 1 }
    | otherwise = return $ world { state = CfgMenu (n + 1) }
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) 
    world@(World _ (CfgMenu n) _ c _ _ _)
    | n == 1 = return $ world { state = CfgMenu (num c) }
    | otherwise = return $ world { state = CfgMenu (n - 1) }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u (CfgMenu n) o c m p a) = case m of
    1 -> return $ world { universe = halfToAlive $ loadConfig $ 
                                        (list c) !! (n - 1)
                        , state = Generator }
    2 -> return $ World u Generator o c 1 p a
    3 -> exitSuccess
--ObjMenu Events
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) 
    world@(World _ (ObjMenu n c) o _ _ _ _)
    | n == (num o) = return $ world { state = ObjMenu 1 c }
    | otherwise = return $ world { state = ObjMenu (n + 1) c }
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) 
    world@(World _ (ObjMenu n c) o _ _ _ _)
    | n == 1 = return $ world { state = ObjMenu (num o) c }
    | otherwise = return $ world { state = ObjMenu (n - 1) c }
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@(World u (ObjMenu _ _) o c m p a) = case m of
    1 -> return $ World u Generator o c 1 p a
    2 -> exitSuccess
--ESC
handler (EventKey (SpecialKey KeyEsc) Down _ _) world = exitSuccess
--F2
handler (EventKey (SpecialKey KeyF2) Down _ _) world = saveWorld world
--default
handler _ w = return w

-- | Render a picture
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
                                            (list c) !! (n - 1)
                                        ObjMenu n coords -> loadObject u 
                                            ((list o) !! (n - 1)) coords
                                  in translate offsetX offsetY $
                                     pictures [(drawUniverse uni), 
                                               (drawMenu a), menu]

-- | Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                 (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                 (u ! (x, y)) 
                 | x <- [1 .. size], y <- [1 .. size]]

-- | Drawing menu background
drawMenu :: Integer -> Picture
drawMenu age = translate (1.5*h) (h/2) $ pictures [color (greyN 0.7) $ 
                    rectangleSolid h h, rectangleWire h h,
                    translate (-100) (h/2 - 40) $ Scale 0.3 0.3 $ 
                    Text $ "Iteration: " ++  (show age)]
               where h = fromIntegral windowHeight

-- * Drawing menu items

-- | for generator state
drawMenu1 :: Int -> [Picture] -> Picture
drawMenu1 m pic = let j = case m of
                        1 -> h + 150
                        2 -> h + 90
                        3 -> h + 30
                        4 -> h - 30
                        5 -> h - 90
                        6 -> h - 150
                      i = case m of
                        1 -> getWidth (pic !! 0) / 2
                        2 -> getWidth (pic !! 1) / 2
                        3 -> getWidth (pic !! 2) / 2
                        4 -> getWidth (pic !! 3) / 2
                        5 -> getWidth (pic !! 4) / 2
                        6 -> getWidth (pic !! 5) / 2
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

-- | for iterator state
drawMenu2 :: Int -> [Picture] -> Picture
drawMenu2 m pic = let j = if m == 1 then h + 30 else h - 30
                      i = getWidth (pic !! 8) / 2
                  in pictures [translate w (h + 30) $ pic !! 8,
                               translate w (h - 30) $ pic !! 5,
                               translate (w - 30 - i) j $ pic !! 11,
                               translate (w + 30 + i) j $ pic !! 12]
                  where w = 1.5 * (fromIntegral windowHeight)
                        h = (fromIntegral windowHeight) / 2

-- | for CfgMenu state
drawMenu3 :: Int -> [Picture] -> Int -> Configs -> Picture
drawMenu3 m pic n c = let j = case m of
                                1 -> h - 60
                                2 -> h - 120
                                3 -> h - 180
                          i = if m == 1 then getWidth (pic !! 6) / 2
                                        else getWidth (pic !! 7) / 2
                      in pictures [translate w (h - 60) $ pic !! 6,
                                   translate w (h - 120) $ pic !! 7,
                                   translate w (h - 180) $ pic !! 5,
                                   translate (w - 200) (h + 10) $ pic !! 9,
                                   translate (w + 200) (h + 10) $ pic !! 10,
                                   translate (w - 30 - i) j $ pic !! 11,
                                   translate (w + 30 + i) j $ pic !! 12,
                                   translate (w - 75) (h + 50) $ Scale 0.2 0.2 $
                                    Text $ "Config " ++ show n ++ "/" ++ 
                                                        show (num c),
                                   translate (w - 80) h $ Scale 0.1 0.1 $
                                    Text $ name $ (list c) !! (n - 1)]
                      where w = 1.5 * (fromIntegral windowHeight)
                            h = (fromIntegral windowHeight) / 2

-- | for ObjMenu state
drawMenu4 :: Int -> [Picture] -> Int -> Objects -> Picture
drawMenu4 m pic n o = let j = if m == 1 then h - 60 else h - 120
                          i = getWidth (pic !! 7) / 2
                      in pictures [translate w (h - 60) $ pic !! 7,
                                   translate w (h - 120) $ pic !! 5,
                                   translate (w - 120) (h + 10) $ pic !! 9,
                                   translate (w + 120) (h + 10) $ pic !! 10,
                                   translate (w - 30 - i) j $ pic !! 11,
                                   translate (w + 30 + i) j $ pic !! 12,
                                   translate (w - 75) (h + 50) $ Scale 0.2 0.2 $
                                    Text $ "Object " ++ show n ++ "/" ++ 
                                                        show (num o),
                                   translate (w - 40) h $ Scale 0.2 0.2 $
                                    Text $ name $ (list o) !! (n - 1)]
                      where w = 1.5 * (fromIntegral windowHeight)
                            h = (fromIntegral windowHeight) / 2

-- | Create picture of dead (empty rectangle) or alive (solid rectangle) cell
drawCell :: Float -> Float -> Cell -> Picture
drawCell x y cell = let figure = case cell of
                                 Dead -> rectangleWire c c
                                 Alive -> rectangleSolid c c
                                 Half -> color (greyN 0.4) $ rectangleSolid c c
                    in translate (cellSize / 2 + cellSize * x)
                                 (cellSize / 2 + cellSize * y)
                                 figure
                    where c = cellSize

