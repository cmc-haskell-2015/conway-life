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
readInts = F.foldr checkInts (pure [])

checkInts :: (Maybe Int, Maybe Int) -> Maybe Location -> Maybe Location
checkInts (Just x, Just y) Nothing = Nothing
checkInts (Just x, Just y) acc = (pure [(x, y)]) <> acc
checkInts _ _ = Nothing

saveWorld :: World -> IO World
saveWorld (World u s o c x y) = do
    saveUni u
    return (World u s o c x y)

saveUni :: Universe -> IO ()
saveUni u = do
    t <- getCurrentTime
    let name = "database/configs/cfg" ++ show t
    writeFile name (makeConfig u)
    putStrLn $ "Configuration saved to " ++ name

makeConfig :: Universe -> String
makeConfig u = getAlive $ F.foldr (++) [] (f cols)
               where cols = map (zip [1..]) (toLists u)
                     f = zipWith (\x y -> map (\(p, q) -> (x, p, q)) y) [1..]

getAlive :: [(Int, Int, Cell)] -> String
getAlive l = F.foldMap (\(x, y, _) -> show x ++ " " ++ show y ++ "\n") alive
             where alive = filter (\(_, _, c) -> isAlive c) l
