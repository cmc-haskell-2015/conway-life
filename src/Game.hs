-- | In this module the main data types are described along with functions
-- for the game core.
module Game where

import Data.Matrix

-- | Cell coordinates in universe.
type Coords = (Int, Int)

-- | Cell - an atomic part of the world.
data Cell
  = Dead    -- ^ Dead (empty) cell.
  | Alive   -- ^ Alive cell.
  | Half    -- ^ This state is for placing the object from database and is used
            -- for GUI.
  deriving (Show)

-- | Universe is a field (matrix) of cells in certain states.
type Universe = Matrix Cell

-- | Universe size in cells
size :: Int
size = 25

-- * Functions to convert cell states

-- | Convert all cells in 'grey' state, after the chosen object was placed.
halfToAlive :: Universe -> Universe
halfToAlive u = fmap convert u

-- | Convert each cell from 'grey' to alive.
convert :: Cell -> Cell
convert Half = Alive
convert c = c

-- | Turn dead cell into alive and vice versa, the Half cells are not changed.
inverseCell :: Cell -> Cell
inverseCell Dead = Alive
inverseCell Alive = Dead
inverceCell c = c

-- | Check if the cell is alive.
isAlive :: Cell -> Bool
isAlive Dead = False
isAlive _ = True

-- * Functions to update universe on each step

-- | Get Moore neighbourhood of a cell as a list of neighbouring cells.
getNeighbours :: Universe -> Coords -> [Cell]
getNeighbours u (x, y) = [u ! (i, j) | i <- [x - 1 .. x + 1], 
                                    j <- [y - 1 .. y + 1],
                                    i >= 1, i <= (nrows u),
                                    j >= 1, j <= (ncols u),
                                    i /= x || j /= y]

-- | Updates the cell according to the number of its neighbours.
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

-- | Update function for cell.
stepCell :: Universe -> Coords -> Cell
stepCell u (x, y) = stepCellAux (u ! (x, y)) (getNeighbours u (x, y))

-- | Universe updater.
stepUniverse :: Universe -> Universe
stepUniverse oldu = foldr (\pos -> setElem (stepCell oldu pos) pos) oldu coords
  where
    coords = [ (i, j) | i <- [1..nrows oldu], j <- [1..ncols oldu] ]
