module Game where

import Data.Matrix

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

--run :: Universe -> IO ()

--update :: Universe -> Universe
