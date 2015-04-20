module Init where

import Text.Read
import Data.Maybe
import Data.Matrix
import Game

--Create a matrix of dead/alive cells
initUni :: [(Int, Int)] -> Universe
initUni l = if length l > 1 then 
                transpose . fromLists . reverse . map reverse $ 
                                                  (initState x y (tail l)) 
            else defState where
                            x = fst (head l) - 1
                            y = snd (head l) - 1

--Default state in case of empty config file
defState :: Universe
defState = matrix 20 20 glider

--Create one glider in lower left corner
glider :: (Int, Int) -> Cell
glider (i, j) | (i == 2) && (j == 1) || 
                (i == 3) && (j == 2) ||
                (i <= 3) && (j == 3) = Alive
              | otherwise = Dead

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

--Get positions of alive cells
positions :: String -> [(Int, Int)]
positions = getInts . (map words) . lines

--Check input and transform coordinates into (Int, Int) if possible
getInts :: [[String]] -> [(Int, Int)]
getInts = tuples .  checkInput . map (map readMaybe)

--Filters wrong config lines
checkInput :: [[Maybe Int]] -> [[Int]]
checkInput = map (map fromJust) . filter onlyJust

--Checks if list has only (Just Int) elements
onlyJust :: [Maybe Int] -> Bool
onlyJust = and . map isJust

--Convert lists of 2 Ints into tuples
tuples :: [[Int]] -> [(Int, Int)]
tuples = foldr (\x acc -> if length x == 2 then (x!!0, x!!1) : acc else acc) []
