module Main where

import           Lib
import           Data.Set                       ( fromList )
import           Data.Maybe                     ( fromMaybe )

-- TODO: Commandline options to specify starting board

main :: IO ()
main = displayGame . fromMaybe emptyBoard . getNamedBoard $ "glider"
