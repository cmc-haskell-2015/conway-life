-- | Graphics interface of the programm
module Graphics where

import Data.Matrix
import Data.Monoid
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import Files
import System.Exit

-- | Available states of the world
data State
  = Generator                          -- ^ Generating universe
  | Iterator                           -- ^ Simulating universe
  | CfgMenu Int                        -- ^ Loading configs from DB to universe
  | ObjMenu Int (Maybe Coords)         -- ^ Same for objects

-- | Id for menu items.
type MenuItem = Int

-- | Rectangle specidied by center point and size
data Rect = Rect 
  { center :: Point 
  , rate   :: Vector
  }

-- | World menu
type Menu = [Button]

-- | Button in the menu
data Button = Button
  { btnRect   :: Rect
  , btnImg    :: Picture
  , btnNum    :: Int
  , btnAction :: World -> IO World
  }

-- | All the main data structures put together.
data World = World
  { universe  :: Universe     -- ^ Matrix of cells.
  , state     :: State        -- ^ Current state of the world.
  , obj       :: Objects      -- ^ Loaded objects.
  , cfg       :: Configs      -- ^ Loaded configurations.
  , selected  :: MenuItem     -- ^ Selected menu item which is to be marked in
                              -- menu panel.
  , menues    :: [Menu]       -- ^ Menu panels.
  , age       :: Integer      -- ^ Number of game iteration.
  , pic       :: [Picture]    -- ^ Menu selectors.
  }

-- | Used in handling mouse events
data MyMouseEvent
    = Move -- ^ when mouse moves
    | Click -- ^ when pressing left mouse button

-- | Save current world as configuration.
saveWorld :: World -> IO World
saveWorld world = do
    saveUni (universe world)
    return world

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
windowWidth :: Float
windowWidth = fromIntegral $ fst windowSize

-- | Window height
windowHeight :: Float
windowHeight = fromIntegral $ snd windowSize

-- | 'Convert' MyMouseEvent to Bool
isClick :: MyMouseEvent -> Bool
isClick Click = True
isClick Move = False

-- | Get active menu from world
activeMenu :: World -> Menu
activeMenu world@World{ state = s, menues = m } = case s of
        Generator -> m !! 0
        Iterator -> m !! 1
        CfgMenu _ -> m !! 2
        ObjMenu _ _ -> m !! 3

-- | Update the universe on each step
updater :: Float -> World -> World
updater _ world@World{ universe = u, state = Iterator, age = a } = world 
                                               { universe = stepUniverse u
                                               , age = a + 1 
                                               }
updater _ w = w

-- | Create button
mkButton :: FilePath -> Int -> Point -> (World -> IO World) -> IO Button
mkButton file num center action = do
  img <- loadBMP file
  case img of
    Bitmap w h _ _ -> do
      return (Button (Rect center (fromIntegral w,fromIntegral h)) img num action)
    _ -> exitFailure

-- | Create generator menu
mkGeneratorMenu :: IO Menu
mkGeneratorMenu = sequence
  [ mkButton "img/start.bmp" 1 (w, h + 150) (\world -> return world { state = Iterator, selected = 1 })
  , mkButton "img/loadcfg.bmp" 2 (w, h + 90) (\world -> return world { state = CfgMenu 1, selected = 1 })
  , mkButton "img/loadobj.bmp" 3 (w, h + 30) (\world -> return world { state = ObjMenu 1 Nothing, selected = 1 })
  , mkButton "img/clear.bmp" 4 (w, h - 30) (\world -> return world { universe = defState })
  , mkButton "img/save.bmp" 5 (w, h - 90) (\world -> saveWorld world)
  , mkButton "img/exit.bmp" 6 (w, h - 150) (\_ -> exitSuccess) ]
  where
    w = 1.5 * windowHeight
    h = windowHeight / 2

-- | Create iterator menu
mkIteratorMenu :: IO Menu
mkIteratorMenu = sequence
  [ mkButton "img/stop.bmp" 1 (w, h + 30) (\world -> return world { state = Generator, selected = 1 })
  , mkButton "img/exit.bmp" 2 (w, h - 30) (\_ -> exitSuccess) ]
  where
    w = 1.5 * windowHeight
    h = windowHeight / 2

-- | Create config menu
mkCfgMenu :: IO Menu
mkCfgMenu = sequence
  [ mkButton "img/select.bmp" 1 (w, h - 60) selectConfig
  , mkButton "img/back.bmp" 2 (w, h - 120) (\world -> return world { state = Generator, selected = 1 })
  , mkButton "img/exit.bmp" 3 (w, h - 180) (\_ -> exitSuccess)
  , mkButton "img/left.bmp" (-1) (w - 200, h + 10) (\world -> return $ cfgMenuNavigation world (-1))
  , mkButton "img/right.bmp" (-2) (w + 200, h + 10) (\world -> return $ cfgMenuNavigation world 1) ]
  where
    w = 1.5 * windowHeight
    h = windowHeight / 2

selectConfig :: World -> IO World
selectConfig world@World{ state = CfgMenu n } = return world { universe = halfToAlive $ loadConfig $ 
                                                   	           (list (cfg world)) !! (n - 1)
                                                             , state = Generator
                                                             , selected = 1 }
selectConfig _ = exitFailure

-- | Create object menu
mkObjMenu :: IO Menu
mkObjMenu = sequence
  [ mkButton "img/back.bmp" 1 (w, h - 60) (\world -> return world { state = Generator, selected = 1 })
  , mkButton "img/exit.bmp" 2 (w, h - 120) (\_ -> exitSuccess)
  , mkButton "img/left.bmp" (-1) (w - 120, h + 10) (\world -> return $ objMenuNavigation world (-1))
  , mkButton "img/right.bmp" (-2) (w + 120, h + 10) (\world -> return $ objMenuNavigation world 1) ]
  where
    w = 1.5 * windowHeight
    h = windowHeight / 2

-- | Check if coords inside rectangle
insideRect :: Point -> Rect -> Bool
insideRect (x, y) (Rect (cx, cy) (w, h)) = x >= cx - w / 2 && x <= cx + w / 2 &&
                                           y >= cy - h / 2 && y <= cy + h / 2

-- | Function for menu navigation by the up & down keys
-- Int parameter is step and should be 1 or -1
keyMenuNavigation :: World -> Int -> World
keyMenuNavigation world step
    | m + step < 1 = world { selected = n }
    | m + step > n = world { selected = 1 }
    | otherwise = world { selected = m + step }
    where m = selected world
          n = length $ filter (\btn-> btnNum btn > 0) (activeMenu world)

-- | Change configs by left & right keys
cfgMenuNavigation :: World -> Int -> World
cfgMenuNavigation world@World{ state = CfgMenu n, cfg = c } step
    | n + step < 1 = world { state = CfgMenu (num c) }
    | n + step > (num c) = world { state = CfgMenu 1 }
    | otherwise = world { state = CfgMenu (n + step) }
cfgMenuNavigation w _ = w

-- | Change objects by left & right keys
objMenuNavigation :: World -> Int -> World
objMenuNavigation world@World{ state = ObjMenu n x, obj = o } step
    | n + step < 1 = world { state = ObjMenu (num o) x }
    | n + step > (num o) = world { state = ObjMenu 1 x }
    | otherwise = world { state = ObjMenu (n + step) x }
objMenuNavigation w _ = w

-- | Handle buttons events
handleMenu :: MyMouseEvent -> Point -> World -> IO World
handleMenu e (x, y) w = 
  case find (\btn -> (nx, ny) `insideRect` btnRect btn) menu of
    Just btn -> case e of
        Click -> btnAction btn w
        Move  -> if btnNum btn > 0 then return w { selected = btnNum btn }
                                   else return w
    Nothing -> handleMouse e (nx, ny) w
  where menu = activeMenu w
        nx = x + windowWidth / 2
        ny = y + windowHeight / 2

-- | Handle mouse events
handleMouse :: MyMouseEvent -> Point -> World -> IO World
handleMouse e (x, y) world@World{ universe = u, state = s, obj = o }
    | (i <= size) && (i >= 1) && (j <= size) && (j >= 1) && (isClick e) && isGenerator = 
	return world { universe = (setElem (inverseCell $ u ! (i, j)) (i, j) u) }
    | (objMenu >= 0) && (isClick e) = return world { universe = halfToAlive $ 
        loadObject u ((list o) !! (objMenu - 1)) (Just (i, j)) }
    | (objMenu >= 0) = return world { state = ObjMenu objMenu (Just (i, j)) }
    | otherwise = return world
    where i = round ((x + cellSize / 2) / cellSize)
          j = round ((y + cellSize / 2) / cellSize)
	  isGenerator = case s of
		Generator -> True
		_ -> False
	  objMenu = case s of
		ObjMenu n _ -> n
		_ -> -1		

-- | Handle events from mouse and keyboard
handler :: Event -> World -> IO World
-- Mouse Events
-- Click
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) world = handleMenu Click (x, y) world
-- Move
handler (EventMotion (x, y)) world = handleMenu Move (x, y) world
-- Keyboard Events
-- Key down for menu navigation
handler (EventKey (SpecialKey KeyDown) Down _ _) world = 
    return $ keyMenuNavigation world 1
-- Key up for menu navigation
handler (EventKey (SpecialKey KeyUp) Down _ _) world = 
    return $ keyMenuNavigation world (-1)
--Enter
handler (EventKey (SpecialKey KeyEnter) Down _ _) 
    world@World { state = s, selected = sel, menues = m } = btnAction (menu !! (sel - 1)) world
  where menu = activeMenu world
--CfgMenu Events
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) world@World{ state = CfgMenu n } = return $ cfgMenuNavigation world 1
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) world@World{ state = CfgMenu n } = return $ cfgMenuNavigation world (-1)
--ObjMenu Events
--Key right
handler (EventKey (SpecialKey KeyRight) Down _ _) world@World{ state = ObjMenu n _ } = return $ objMenuNavigation world 1
--Key left
handler (EventKey (SpecialKey KeyLeft) Down _ _) world@World{ state = ObjMenu n _ } = return $ objMenuNavigation world (-1)
--ESC
handler (EventKey (SpecialKey KeyEsc) Down _ _) world = exitSuccess
--F2
handler (EventKey (SpecialKey KeyF2) Down _ _) world = saveWorld world
--default
handler _ w = return w

-- | Render a picture
renderer :: World -> Picture
renderer w@(World u s o c sel m a p) = let offsetX = - windowWidth / 2
                                           offsetY = - windowHeight / 2
                                           uni = case s of
                                        	Generator -> u
                                        	Iterator -> u
                                        	CfgMenu n -> loadConfig $ 
                                            		(list c) !! (n - 1)
                                        	ObjMenu n coords -> loadObject u 
                                            		((list o) !! (n - 1)) coords
                                        in translate offsetX offsetY $
                                     	  pictures [(drawUniverse uni), (drawMenuBack a), (drawMenu w), (drawSelectors w)]

-- | Create a picture as a list (superposition) of cells 
drawUniverse :: Universe -> Picture
drawUniverse u = pictures [drawCell 
                 (fromIntegral (x - 1)) (fromIntegral (y - 1)) 
                 (u ! (x, y)) 
                 | x <- [1 .. size], y <- [1 .. size]]

-- | Drawing menu background
drawMenuBack :: Integer -> Picture
drawMenuBack age = translate (1.5*h) (h/2) $ pictures [color (greyN 0.7) $ rectangleSolid h h
						      , rectangleWire h h
                    				      , translate (-100) (h/2 - 40) $ Scale 0.3 0.3 $ 
                    						Text $ "Iteration: " ++  (show age)]
	where h = windowHeight

-- | Drawing menu buttons
drawButtons :: Menu -> Picture
drawButtons menu = pictures $ map drawButton menu

-- | Draw one button
drawButton :: Button -> Picture
drawButton (Button (Rect (x, y) _) img _ _) = translate x y img

-- | Drawing full menu with buttons and text
drawMenu :: World -> Picture
drawMenu world@World{ state = s, obj = o, cfg = c } = 
  let labels = case s of
    	CfgMenu n -> [ translate (w - 75) (h + 50) $ Scale 0.2 0.2 $ Text $ "Config " ++ show n ++ "/" ++ show (num c)
                     , translate (w - 80) h $ Scale 0.1 0.1 $ Text $ name $ (list c) !! (n - 1) ]
    	ObjMenu n _ -> [ translate (w - 75) (h + 50) $ Scale 0.2 0.2 $ Text $ "Object " ++ show n ++ "/" ++ show (num o)
                     , translate (w - 40) h $ Scale 0.2 0.2 $ Text $ name $ (list o) !! (n - 1)]
    	_ -> [] 
  in pictures $ [drawButtons menu] ++ labels
  where 
    w = 1.5 * windowHeight
    h = windowHeight / 2
    menu = activeMenu world

-- | Drawing menu selectors
drawSelectors :: World -> Picture
drawSelectors world@World{ selected = s, pic = p } = pictures [ translate (x - width / 2 - 30) y (p !! 0)
							      , translate (x + width / 2 + 30) y (p !! 1) ]
  where 
    x = fst . center. btnRect $ (activeMenu world) !! (s - 1)
    y = snd . center. btnRect $ (activeMenu world) !! (s - 1)
    width = fst . rate. btnRect $ (activeMenu world) !! (s - 1)

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

