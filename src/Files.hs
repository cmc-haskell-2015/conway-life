-- | This module consists of functions for database processing: reading from
-- configuration files and saving new configurations.
module Files where

import Data.Maybe
import Data.Monoid
import Data.Matrix
import qualified Data.Foldable as F
import Data.Time
import Text.Read
import Control.Applicative
import Game

-- | File contents.
type Content = String

-- | Location of the object is a list of the coordinates of those cells that 
-- form it.
type Location = [Coords]

-- | Name of an object/configuration.
type Name = String

-- | Object loaded from config file.
data Object = Object 
  { name    :: Name       -- ^ Name of loaded config.
  , coords  :: Location   -- ^ Loction of the object.
  } deriving (Show)

-- | Loaded separate objects.
data Objects = Objects 
  { list :: [Object]   -- ^ The list of objects itself.
  , num  :: Int        -- ^ The length of the list == number of objects.
  }

-- | Similar to Objects, for whole configurations.
type Configs = Objects

-- | Default state.
defState :: Universe
defState = matrix size size ( \ _ -> Dead )

-- * Functions to convert object and config coords to universe

-- | convert object (interpreted as object) to universe
loadObject :: Universe -> Object -> Maybe Coords -> Universe
loadObject u _ Nothing = u
loadObject u obj (Just mouse) = foldr (setElem Half) u (filter insideUniverse cs)
  where
    Object _ cs = placeObject obj mouse
    insideUniverse (x, y) = x <= size && x >= 1 && y <= size && y >= 1

placeObject :: Object -> Coords -> Object
placeObject (Object name cs) mouse = Object name $ map (placePoint mouse (cx, cy)) cs
  where
    cx = (maximum $ map fst cs) `div` 2
    cy = (maximum $ map snd cs) `div` 2

placePoint :: Coords  -- ^ mouse position
           -> Coords  -- ^ object center
           -> Coords  -- ^ point
           -> Coords
placePoint (mx, my) (cx, cy) (x, y) = (x + mx - cx, y + my - cy)

-- | convert object (interpeted as config) to universe
loadConfig :: Object -> Universe
loadConfig obj = foldr (\coords u -> setElem Half coords u) defState (coords obj)

-- * Reading files from database

-- | Remove ./ and ../ from directory contents and extend path of files.
onlyFiles :: String -> [String] -> [Name]
onlyFiles dir = map (("database/" ++ dir ++ "/") ++)
                . filter (\x -> (x /= ".") && (x /= ".."))

-- | Shorten file name, dropping directory names.
shortName :: Name -> Name
shortName s = iterate (tail . dropWhile (/= '/')) s !! 2

-- | Get objects from database.
initObjects :: [Name] -> [Content] -> IO [Object]
initObjects n c = objList $ zip n (getCoords <$> c)

-- | Create list of objects out of proper configurations.
objList :: [(Name, Maybe Location)] -> IO [Object]
objList = foldr catchErrors (return [])
    
-- | Find configs with errors, exclude them from objects list
-- and write messages about it.
catchErrors :: (Name, Maybe Location) -> IO [Object] -> IO [Object]
catchErrors (n, Nothing) acc = do 
    putStrLn $ "Error reading database: file '" ++ n ++ "' wasn't loaded"
    acc
catchErrors (n, Just []) acc = do 
    putStrLn $ "Error reading database: file '" ++ n ++ "' wasn't loaded"
    acc
catchErrors (n, Just c) acc = (:) <$> (return (Object n c)) <*> acc

-- | Read file content.
getCoords :: Content -> Maybe Location
getCoords s | odd (length s1) = Nothing
            | otherwise = readInts . makeTuples $ s1
                  where s1 = lines s >>= words

-- | Make tuples of Ints.
makeTuples :: [String] -> [(Maybe Int, Maybe Int)]
makeTuples (x : y : ys) = (readMaybe x, readMaybe y) : makeTuples ys
makeTuples _ = []

-- | Try to read Ints.
readInts :: [(Maybe Int, Maybe Int)] -> Maybe Location
readInts = F.foldr checkInts (pure [])

-- | Check if all coords have proper format.
checkInts :: (Maybe Int, Maybe Int) -> Maybe Location -> Maybe Location
checkInts (Just x, Just y) (Just acc) = (pure [(x, y)]) <> (Just acc)
checkInts _ _ = Nothing

-- * Saving files to database

-- | Create new file with unique name and save configuration.
saveUni :: Universe -> IO ()
saveUni u = do
    t <- getCurrentTime
    let name = "database/configs/cfg" ++ show t
    writeFile name (makeConfig u)
    putStrLn $ "Configuration saved to " ++ name

-- | Make config file out of matrix.
makeConfig :: Universe -> Content
makeConfig u = getAlive $ F.foldr (++) [] (f cols)
               where cols = map (zip [1..]) (toLists u)
                     f = zipWith (\x y -> map (\(p, q) -> (x, p, q)) y) [1..]

-- | Convert coords of alive cells to string.
getAlive :: [(Int, Int, Cell)] -> Content
getAlive l = F.foldMap (\(x, y, _) -> show x ++ " " ++ show y ++ "\n") alive
             where alive = filter (\(_, _, c) -> isAlive c) l
