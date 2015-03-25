module Main where

import Control.Exception

data Cell = Dead | Alive

data Universe = Universe [[Cell]]

--initState :: [(Int, Int)] -> Universe

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
      Right content -> putStrLn . show . positions $ content
