module Lib
    ( Board(..)
    , neighbourGrid
    , countNeighbours
    , survives
    , stepBoard
    )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.Tuple.Extra               ( both )
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( guard )

type Board = Set (Int, Int)

neighbourGrid :: (Int, Int) -> [(Int, Int)]
neighbourGrid (x, y) = do
    x' <- [x - 1 .. x + 1]
    y' <- [y - 1 .. y + 1]
    guard (x' /= x || y' /= y)
    return (x', y')

countNeighbours :: Board -> (Int, Int) -> Int
countNeighbours board position = length liveIndicators
  where
    liveIndicators = filter id indicatorList
    indicatorList  = map (`S.member` board) grid
    grid           = neighbourGrid position

survives :: Board -> (Int, Int) -> Bool
survives board position =
    let neighbours = countNeighbours board position
        alive      = S.member position board
    in  (neighbours == 3) || (alive && neighbours == 2)

stepBoard :: Board -> Board
stepBoard board = S.filter (survives board) board

getBounds :: Board -> ((Int, Int), (Int, Int))
getBounds board =
    let cellXs = S.map fst board
        cellYs = S.map snd board
        minX   = fromMaybe 0 $ S.lookupMin cellXs
        maxX   = fromMaybe 0 $ S.lookupMax cellXs
        minY   = fromMaybe 0 $ S.lookupMin cellYs
        maxY   = fromMaybe 0 $ S.lookupMax cellYs
    in  ((minX, minY), (maxX, maxY))
