module Init where

import Data.Maybe
import Data.Matrix
import Game

biloaf = [(2, 3), (2, 4), (3, 2), (3, 5), (4, 3), (4, 5), (5, 4), (5, 6),
          (5, 7), (6, 5), (6, 8), (7, 6), (7, 8), (8, 7)]
clock = [(1, 5), (1, 6), (2, 5), (2, 6), (4, 5), (4, 6), (4, 7), (4, 8),
         (5, 4), (5, 9), (5, 11), (5, 12), (6, 4), (6, 6), (6, 7), (6, 9),
         (6, 11), (6, 12), (7, 1), (7, 2), (7, 4), (7, 5), (7, 9), (8, 1),
         (8, 2), (8, 4), (8, 9), (9, 5), (9, 6), (9, 7), (9, 8), (11, 7),
         (11, 8), (12, 7), (12, 8)]
cross = [(2, 4), (2, 5), (2, 6), (2, 7), (3, 4), (3, 7), (4, 2), (4, 3),
         (4, 4), (4, 7), (4, 8), (4, 9), (5, 2), (5, 9), (6, 2), (6, 9),
         (7, 2), (7, 3), (7, 4), (7, 7), (7, 8), (7, 9), (8, 4), (8, 7),
         (9, 4), (9, 5), (9, 6), (9, 7)]
fumarole = [(1, 1), (1, 2), (1, 7), (1, 8), (2, 1), (2, 3), (2, 6), (2, 8),
            (3, 3), (3, 6), (4, 2), (4, 7), (5, 2), (5, 3), (5, 6), (5, 7),
            (6, 4), (6, 5)]
galaxy = [(3, 9), (3, 10), (4, 10), (5, 5), (5, 6), (5, 7), (5, 10), (5, 11),
          (6, 3), (6, 4), (6, 5), (6, 7), (6, 11), (7, 3), (7, 8), (7, 10),
          (7, 11), (8, 7), (8, 9), (9, 5), (9, 6), (9, 8), (9, 13), (10, 5),
          (10, 9), (10, 11), (10, 12), (10, 13), (11, 5), (11, 6), (11, 9),
          (11, 10), (11, 11), (12, 6), (13, 6), (13, 7)]
glider :: [(Int, Int)]
glider = [(10, 3), (9, 1), (9, 3), (8, 2), (8, 3)]

--Create a matrix of dead/alive cells
loadState :: Universe -> [(Int, Int)] -> Universe
loadState u l = fromLists . reverse . map reverse $
                                (initState (nrows u - 1) (ncols u - 1) l) 

--Default state
defState :: Int -> Int -> Universe
defState width height = matrix width height ( \ _ -> Dead )

--Make list of lists of cells to be later transformed into matrix
initState :: Int -> Int -> [(Int, Int)] -> [[Cell]]
initState (-1) _ _ = []
initState x y l = (initRow x y l) : (initState (x-1) y l)

--Make row of cells
initRow :: Int -> Int -> [(Int, Int)] -> [Cell]
initRow _ (-1) _ = []
initRow x y l
        | elem (x+1, y+1) l = Alive : initRow x (y-1) l
        | otherwise = Dead : initRow x (y-1) l
