module Init where

import Data.Maybe
import Data.Matrix
import Text.Read
import Game

--Constant field size
size :: Int
size = 25

--Default state
defState :: Universe
defState = matrix size size ( \ _ -> Dead )

onlyFiles :: String -> [String] -> [String]
onlyFiles dir = map (("database/" ++ dir ++ "/") ++)
                . filter (\x -> (x /= ".") && (x /= ".."))

--Get objects from DB
initObjects :: [String] -> Objects
initObjects _ = Objects [] 0

initConfigs :: [String] -> Configs
initConfigs _ = Configs [] 0
