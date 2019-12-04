module Main where

import           Lib
import           Data.Set                       ( fromList )
import           Data.Maybe                     ( fromMaybe )

-- TODO: Commandline options to specify starting board and other parameters
main :: IO ()
main =
    let bounds = Bounds { minX = -10, minY = -5, maxX = 10, maxY = 5 }
    in  animateGame bounds . fromMaybe emptyBoard . getNamedBoard $ "glider"
