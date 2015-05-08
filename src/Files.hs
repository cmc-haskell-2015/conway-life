module Files where

import Data.Maybe
import Data.Monoid
import Data.Matrix
import qualified Data.Foldable as F
import Data.Time
import Text.Read
import Control.Applicative
import Game

type Content = String

--Remove ./ and ../ from dir contents and extend path of files
onlyFiles :: String -> [String] -> [Name]
onlyFiles dir = map (("database/" ++ dir ++ "/") ++)
                . filter (\x -> (x /= ".") && (x /= ".."))

--Shorten file name, dropping dir names
shortName :: Name -> Name
shortName s = iterate (tail . dropWhile (/= '/')) s !! 2

--Get objects from DB
initObjects :: [Name] -> [Content] -> IO [Object]
initObjects n c = catchErrors $ zip n (getCoords <$> c)

--Find configs with errors, remove them from list and write msgs about it
catchErrors :: [(Name, Maybe Location)] -> IO [Object]
catchErrors [] = return []
catchErrors ((n, Nothing) : cdr) = do 
    putStrLn $ "Error reading database: file '" ++ n ++ "' wasn't loaded"
    catchErrors cdr
catchErrors ((n, Just []) : cdr) = do 
    putStrLn $ "Error reading database: file '" ++ n ++ "' wasn't loaded"
    catchErrors cdr
catchErrors ((n, Just c) : cdr) = (:) <$> (return (Object n c)) 
                                      <*> catchErrors cdr
                                   

--Read file content
getCoords :: Content -> Maybe Location
getCoords s | odd (length s1) = Nothing
            | otherwise = readInts . makeTuples $ s1
                  where s1 = lines s >>= words

--Make tuples of ints
makeTuples :: [String] -> [(Maybe Int, Maybe Int)]
makeTuples (x : y : ys) = (readMaybe x, readMaybe y) : makeTuples ys
makeTuples _ = []

--Try to read ints
readInts :: [(Maybe Int, Maybe Int)] -> Maybe Location
readInts = F.foldr checkInts (pure [])

--Check if all coords have proper format
checkInts :: (Maybe Int, Maybe Int) -> Maybe Location -> Maybe Location
checkInts (Just x, Just y) (Just acc) = (pure [(x, y)]) <> (Just acc)
checkInts _ _ = Nothing

--Save current uni as config
saveWorld :: World -> IO World
saveWorld wor = do
    saveUni (universe wor)
    return wor

--Create new file with unique name and save config
saveUni :: Universe -> IO ()
saveUni u = do
    t <- getCurrentTime
    let name = "database/configs/cfg" ++ show t
    writeFile name (makeConfig u)
    putStrLn $ "Configuration saved to " ++ name

--Make config file out of matrix
makeConfig :: Universe -> String
makeConfig u = getAlive $ F.foldr (++) [] (f cols)
               where cols = map (zip [1..]) (toLists u)
                     f = zipWith (\x y -> map (\(p, q) -> (x, p, q)) y) [1..]

--Convert coords of alive cells to string
getAlive :: [(Int, Int, Cell)] -> String
getAlive l = F.foldMap (\(x, y, _) -> show x ++ " " ++ show y ++ "\n") alive
             where alive = filter (\(_, _, c) -> isAlive c) l
