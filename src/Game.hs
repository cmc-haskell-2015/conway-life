module Game where

import Data.Matrix

data Cell = Dead | Alive deriving (Show)

data Universe = Universe (Matrix Cell) deriving (Show)
