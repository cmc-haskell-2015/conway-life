module Init where

import Data.Maybe
import Data.Matrix
import Text.Read
import Control.Applicative
import Game

type Name = String

type Content = String

--Constant field size
size :: Int
size = 25

--Default state
defState :: Universe
defState = matrix size size ( \ _ -> Dead )

onlyFiles :: String -> [String] -> [Name]
onlyFiles dir = map (("database/" ++ dir ++ "/") ++)
                . filter (\x -> (x /= ".") && (x /= ".."))

shortName :: Name -> Name
shortName s = iterate (tail . dropWhile (/= '/')) s !! 2

--Get objects from DB
initObjects :: [(Name, Content)] -> Objects
initObjects l = Objects (map object l) (length l)

initConfigs :: [(Name, Content)] -> Configs
initConfigs l = Configs (map object l) (length l)

object :: (Name, Content) -> Object
object (n, c) = Object n (getCoords c)

getCoords :: Content -> Location
getCoords s = readInts <$> makeTuples s1
                  where s1 = lines s >>= words

makeTuples :: [String] -> [(String, String)]
makeTuples (x : y : ys) = (x, y) : makeTuples ys
makeTuples _ = []

readInts :: (String, String) -> (Int, Int)
readInts (x, y) = (read x, read y)
