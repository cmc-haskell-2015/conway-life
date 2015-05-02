module Game where

import Data.Matrix

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

type Coords = (Int, Int)

--Right for generator, Left for simple universe
data World = World
                { universe :: (Either Universe Universe) 
                , obj :: Objects 
                , cfg :: Configs }    

data Objects = Objects

data Configs = Configs

inverseCell :: Cell -> Cell
inverseCell Dead = Alive
inverseCell Alive = Dead

--Check if the cell is alive
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

--Get Moore neighbourhood of a cell as a list of neighbouring cells
getNeighbours :: Universe -> Coords -> [Cell]
getNeighbours u (x, y) = [u ! (i, j) | i <- [x - 1 .. x + 1], 
                                    j <- [y - 1 .. y + 1],
                                    i >= 1, i <= (nrows u),
                                    j >= 1, j <= (ncols u),
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

--Update function for cell
stepCell :: Universe -> Coords -> Cell
stepCell u (x, y) = stepCellAux (u ! (x, y)) (getNeighbours u (x, y))

--Auxillary function to update the universe cell by cell
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

--Universe updater
stepUniverse :: Universe -> Universe
stepUniverse u = stepUniverseAux u u (nrows u, ncols u)
