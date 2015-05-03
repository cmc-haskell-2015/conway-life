module Init where

import Data.Maybe
import Data.Matrix
import Text.Read
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
initObjects _ = Objects [] 0

initConfigs :: [(Name, Content)] -> Configs
initConfigs _ = Configs [] 0
