module Files where

import Data.Maybe
import Data.Monoid
import Data.Matrix
import Text.Read
import Control.Applicative
import Game

type Content = String

onlyFiles :: String -> [String] -> [Name]
onlyFiles dir = map (("database/" ++ dir ++ "/") ++)
                . filter (\x -> (x /= ".") && (x /= ".."))

shortName :: Name -> Name
shortName s = iterate (tail . dropWhile (/= '/')) s !! 2

--Get objects from DB
initObjects :: [Name] -> [Content] -> IO [Object]
initObjects n c = catchErrors $ zip n (getCoords <$> c)

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
                                   

getCoords :: Content -> Maybe Location
getCoords s | odd (length s1) = Nothing
            | otherwise = readInts . makeTuples $ s1
                  where s1 = lines s >>= words

makeTuples :: [String] -> [(Maybe Int, Maybe Int)]
makeTuples (x : y : ys) = (readMaybe x, readMaybe y) : makeTuples ys
makeTuples _ = []

readInts :: [(Maybe Int, Maybe Int)] -> Maybe Location
readInts = foldr checkInts (pure [])

checkInts :: (Maybe Int, Maybe Int) -> Maybe Location -> Maybe Location
checkInts (Just x, Just y) Nothing = Nothing
checkInts (Just x, Just y) acc = (pure [(x, y)]) <> acc
checkInts _ _ = Nothing

saveWorld :: World -> IO World
saveWorld (World u o c) = do
    saveUni . getUni $ u
    return (World u o c)

saveUni :: Universe -> IO ()
saveUni = undefined

getUni :: Either Universe Universe -> Universe
getUni (Left u) = u
getUni (Right u) = u
