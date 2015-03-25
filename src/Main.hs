module Main where

import Control.Exception

data Cell = Dead | Alive deriving (Show)

data Universe = Universe [[Cell]] deriving (Show)

initUni :: [(Int, Int)] -> Universe
initUni l =  Universe . reverse . map reverse $ 
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
        | elem (x, y) l = Alive : initRow x (y-1) l
        | otherwise = Dead : initRow x (y-1) l
--run :: Universe -> IO()

--update :: Universe -> Universe

positions :: String -> [(Int, Int)]
positions = getInts . (map words) . lines

getInts :: [[String]] -> [(Int, Int)]
getInts = tuples .  map (map read)

tuples :: [[Int]] -> [(Int, Int)]
tuples = foldr (\x acc -> if length x == 2 then (x!!0, x!!1) : acc else acc) []

main :: IO ()
main = do 
    putStrLn "Configuration file:"
    config <- getLine
    result <- try $ readFile config :: IO (Either IOException String)
    case result of
      Left exception -> putStrLn $ "Error: " ++ show exception
      Right content -> putStrLn . show . initUni . positions $ content
