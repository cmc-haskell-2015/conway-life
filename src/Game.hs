module Game where

import Data.Matrix
import qualified Data.Vector as V

data Cell = Dead | Alive deriving (Show)

type Universe = (Matrix Cell)

--run :: Universe -> IO ()

--update :: Universe -> Universe
