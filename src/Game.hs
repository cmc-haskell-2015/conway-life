-- | ???
module Game where

import Data.Matrix
import Graphics.Gloss

-- | ???
data Cell
  = Dead    -- ^ ???
  | Alive   -- ^ ???
  | Half    -- ^ ???
  deriving (Show)

-- | Coords - last mouse position
data State
  = Generator                   -- ^ ???
  | Iterator                    -- ^ ???
  | CfgMenu Int                 -- ^ ???
  | ObjMenu Int (Maybe Coords)  -- ^ ???

-- | ???
type Universe = Matrix Cell

-- | ???
type Coords = (Int, Int)

-- | ???
type Location = [Coords]

-- | ???
type Name = String

-- | ???
type MenuItem = Int

-- | ???
data World = World
  { universe  :: Universe     -- ^ ???
  , state     :: State        -- ^ ???
  , obj       :: Objects      -- ^ ???
  , cfg       :: Configs      -- ^ ???
  , selected  :: MenuItem     -- ^ ???
  , pic       :: [Picture]    -- ^ ???
  , age       :: Integer      -- ^ ???
  }

-- | ???
data Object = Object 
  { name    :: Name       -- ^ ???
  , coords  :: Location   -- ^ ???
  } deriving (Show)

-- | ???
data Objects = Objects 
  { oList :: [Object]   -- ^ ???
  , oNum  :: Int        -- ^ ???
  }

-- | ???
data Configs = Configs
  { cList :: [Object]   -- ^ ???
  , cNum  :: Int        -- ^ ???
  }

-- | Constant field size
size :: Int
size = 25

-- | Default state
defState :: Universe
defState = matrix size size ( \ _ -> Dead )

-- | Functions to convert object and config coords to universe
loadObject :: Universe -> Object -> Maybe Coords -> Universe
loadObject u obj (Just c) = foldr (loadObjectAux (x, y) c) u (coords obj)
                            where x = (maximum $ map fst (coords obj)) `div` 2
                                  y = (maximum $ map snd (coords obj)) `div` 2
loadObject u _ Nothing = u

-- | ???
loadObjectAux :: Coords -> Coords -> Coords -> Universe -> Universe
loadObjectAux (x0, y0) (x1, y1) (x2, y2) u = if (x <= size) && (x >= 1) &&
                                                (y <= size) && (y >= 1) 
                                             then setElem Half (x, y) u
                                             else u
                                             where x = x1 + x2 - x0
                                                   y = y1 + y2 - y0

-- | ???
loadConfig :: Object -> Universe
loadConfig obj = foldr ( \ coords u -> setElem Half coords u) 
                        defState (coords obj)

-- | Functions to convert cell states
halfToAlive :: Universe -> Universe
halfToAlive u = fmap convert u

-- | ???
convert :: Cell -> Cell
convert Half = Alive
convert c = c

-- | ???
inverseCell :: Cell -> Cell
inverseCell Dead = Alive
inverseCell Alive = Dead

-- | Check if the cell is alive
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

-- | Get Moore neighbourhood of a cell as a list of neighbouring cells
getNeighbours :: Universe -> Coords -> [Cell]
getNeighbours u (x, y) = [u ! (i, j) | i <- [x - 1 .. x + 1], 
                                    j <- [y - 1 .. y + 1],
                                    i >= 1, i <= (nrows u),
                                    j >= 1, j <= (ncols u),
                                    i /= x || j /= y]

-- | Updates the cell according to the number of its neighbours
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

-- | Update function for cell
stepCell :: Universe -> Coords -> Cell
stepCell u (x, y) = stepCellAux (u ! (x, y)) (getNeighbours u (x, y))

-- | Auxillary function to update the universe cell by cell
stepUniverseAux :: Universe -> Universe -> Coords -> Universe
stepUniverseAux oldu newu (1, 1) = setElem (stepCell oldu (1, 1)) (1, 1) newu
stepUniverseAux oldu newu (x, 1) = stepUniverseAux 
                                   oldu 
                                   (setElem (stepCell oldu (x, 1)) (x, 1) newu)
                                   ((x - 1), (ncols oldu))
stepUniverseAux oldu newu (x, y) = stepUniverseAux 
                                   oldu 
                                   (setElem (stepCell oldu (x, y)) (x, y) newu)
                                   (x, (y - 1))

-- | Universe updater
stepUniverse :: Universe -> Universe
stepUniverse u = stepUniverseAux u u (nrows u, ncols u)

