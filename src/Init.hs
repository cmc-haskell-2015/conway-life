module Init where

import Data.Maybe
import Data.Matrix
import Text.Read
import Game

size :: Int
size = 25

--Default state
defState :: Universe
defState = matrix size size ( \ _ -> Dead )
