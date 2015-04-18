module Game where

import Data.Matrix
import qualified Data.Vector as V

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

run :: Universe -> IO ()
run uni = do
            print uni --here we must draw it
            next <- getLine
            if (next == "q") then 
                putStrLn "Ok bye"
            else 
                run (update uni)

update :: Universe -> Universe
update u = updateRow u 1 (matrix (nrows u) (ncols u) (\(i,j) -> Dead))

updateRow :: Universe -> Int -> Universe -> Universe
updateRow old r new | r == (nrows old) + 1 = new
                    | otherwise = updateRow old (r+1) (updateCol old r 1 new)

updateCol :: Universe -> Int -> Int -> Universe -> Universe
updateCol old r c new | c == (ncols old) + 1 = new
                      | otherwise = if (border r c old) then 
                                        updateBorder old r c new
                                    else updateCommon old r c new

border :: Int -> Int -> Universe -> Bool
border r c u = (r == 1) || (c == 1) || (r == nrows u) || (c == ncols u)
                                                                

updateBorder :: Universe -> Int -> Int -> Universe -> Universe
updateBorder old i j new = setElem (updateCell newi newj ext) (i, j) new where
    newi = if (i == 1) then 2 else i
    newj = if (j == 1) then 2 else j
    ext = extand i j old

extand :: Int -> Int -> Universe -> Universe
extand i j u | i == 1 = extand 2 j (down <-> u)
             | j == 1 = extand i 2 (right <|> u)
             | i == nrows u = extand i j (u <-> up)
             | j == ncols u = extand i j (u <|> left)
             | otherwise = u
             where
                down = submatrix (nrows u) (nrows u) 1 (ncols u) u
                right = submatrix 1 (nrows u) (ncols u) (ncols u) u
                up = submatrix 1 1 1 (ncols u) u
                left = submatrix 1 (nrows u) 1 1 u

updateCommon :: Universe -> Int -> Int -> Universe -> Universe
updateCommon old i j new = setElem (updateCell i j old) (i, j) new

updateCell :: Int -> Int -> Universe -> Cell
updateCell i j uni = newState (uni!(i,j)) 
                     (neighbours nhood - isAlive (uni!(i,j))) where
                                 nhood = (submatrix (i-1) (i+1) (j-1) (j+1) uni)

neighbours :: Universe -> Int
neighbours nhood = V.foldr (\x acc -> isAlive x + acc) 0
               ((getRow 1 nhood) V.++ (getRow 2 nhood) V.++ (getRow 3 nhood))

isAlive :: Cell -> Int
isAlive Alive = 1
isAlive _ = 0

newState :: Cell -> Int -> Cell
newState Alive 2 = Alive
newState _ 3 = Alive
newState _ _ = Dead
