module Main where

import           Lib
import           Data.Set                       ( fromList )

glider :: Board
glider = fromList [(0, 0), (1, 0), (2, 0), (2, 1), (1, 2)]

blinker :: Board
blinker = fromList [(0, 0), (1, 0), (2, 0)]

toad :: Board
toad = fromList [(0, 0), (1, 0), (2, 0), (1, 1), (2, 1), (3, 1)]

main :: IO ()
main = displayGame toad
