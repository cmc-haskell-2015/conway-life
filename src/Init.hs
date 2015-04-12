module Init where

import Text.Read
import Data.Maybe
import Data.Matrix
import Game

initUni :: [(Int, Int)] -> Universe
initUni l =  Universe . fromLists . reverse . map reverse $ 
    if length l >= 1 then initState x y (tail l) else defState where
                                                         x = fst (head l) - 1
                                                         y = snd (head l) - 1

defState = [[]]

initState :: Int -> Int -> [(Int, Int)] -> [[Cell]]
initState (-1) _ _ = []
initState x y l = (initRow x y l) : (initState (x-1) y l)

initRow :: Int -> Int -> [(Int, Int)] -> [Cell]
initRow _ (-1) _ = []
initRow x y l
        | elem (x+1, y+1) l = Alive : initRow x (y-1) l
        | otherwise = Dead : initRow x (y-1) l

positions :: String -> [(Int, Int)]
positions = getInts . (map words) . lines

getInts :: [[String]] -> [(Int, Int)]
getInts = tuples .  checkInput . map (map readMaybe)

checkInput :: [[Maybe Int]] -> [[Int]]
checkInput = map (map fromJust) . filter onlyJust

onlyJust :: [Maybe Int] -> Bool
onlyJust = and . map isJust

tuples :: [[Int]] -> [(Int, Int)]
tuples = foldr (\x acc -> if length x == 2 then (x!!0, x!!1) : acc else acc) []
