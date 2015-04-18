module Game where

import Data.Matrix

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

--run :: Universe -> IO ()

update :: Universe -> Universe
update u = updateRow u 1 (matrix (nrows u) (ncols u) (\(i,j) -> Dead))

updateRow :: Universe -> Int -> Universe -> Universe
updateRow old r new | r == (nrows old) + 1 = new
                    | otherwise = updateRow old (r+1) (updateCol old r 1 new)

updateCol :: Universe -> Int -> Int -> Universe -> Universe
updateCol old r c new | c == (ncols old) + 1 = new
                      | otherwise = if (border r c) then 
                                        updateBorder old r c new
                                    else updateCommon old r c new

updateBorder :: Universe -> Int -> Int -> Universe -> Universe
updateBorder = undefined

updateCommon :: Universe -> Int -> Int -> Universe -> Universe
updateCommon = undefined

isAlive :: Cell -> Int
isAlive Alive = 1
isAlive _ = 0

newState :: Cell -> Int -> Cell
newState Alive 2 = Alive
newState _ 3 = Alive
newState _ _ = Dead
