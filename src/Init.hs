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

--Get config files for objects from DB
-- TODO: everything
initObjects :: Objects
initObjects = Objects

--Get config files for saved configurations
initConfigs :: Configs
initConfigs = Configs
