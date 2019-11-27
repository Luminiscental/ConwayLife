{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( Board(..)
    , Bounds(..)
    , neighbourGrid
    , countNeighbours
    , survives
    , stepBoard
    , playGame
    , getBounds
    , displayBoard
    , displayGame
    , emptyBoard
    , getNamedBoard
    , translateBoard
    )
where

import           Data.Set                       ( Set )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( intersperse )
import           Control.Monad                  ( guard
                                                , unless
                                                )
import           System.Console.ANSI            ( clearScreen
                                                , hideCursor
                                                , showCursor
                                                , setCursorPosition
                                                )
import           System.IO                      ( stdin
                                                , hWaitForInput
                                                )

type Board = Set (Int, Int)

data Bounds = Bounds { minX :: Int, minY :: Int, maxX :: Int, maxY :: Int }

boundWidth :: Bounds -> Int
boundWidth b = maxX b - minX b

boundHeight :: Bounds -> Int
boundHeight b = maxY b - minY b

extendBounds :: Bounds -> Bounds
extendBounds b = Bounds { minX = minX b - 1
                        , minY = minY b - 1
                        , maxX = maxX b + 1
                        , maxY = maxY b + 1
                        }

emptyBounds :: (Int, Int) -> Bounds
emptyBounds (x, y) = Bounds { minX = x, minY = y, maxX = x, maxY = y }

gridRange :: Bounds -> [(Int, Int)]
gridRange b = do
    x <- [minX b .. maxX b]
    y <- [minY b .. maxY b]
    return (x, y)

neighbourGrid :: (Int, Int) -> [(Int, Int)]
neighbourGrid (x, y) = do
    (x', y') <- gridRange . extendBounds . emptyBounds $ (x, y)
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
stepBoard board =
    let cellsToConsider =
                S.fromList . gridRange . extendBounds . getBounds $ board
    in  S.filter (survives board) cellsToConsider

playGame :: Board -> [Board]
playGame board = board : playGame (stepBoard board)

getBounds :: Board -> Bounds
getBounds board =
    let cellXs = S.map fst board
        cellYs = S.map snd board
        minX   = fromMaybe 0 $ S.lookupMin cellXs
        maxX   = fromMaybe 0 $ S.lookupMax cellXs
        minY   = fromMaybe 0 $ S.lookupMin cellYs
        maxY   = fromMaybe 0 $ S.lookupMax cellYs
    in  Bounds { minX, minY, maxX, maxY }

glider :: Board
glider = S.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (1, 2)]

blinker :: Board
blinker = S.fromList [(0, 0), (1, 0), (2, 0)]

toad :: Board
toad = S.fromList [(0, 0), (1, 0), (2, 0), (1, 1), (2, 1), (3, 1)]

emptyBoard :: Board
emptyBoard = S.empty

namedBoards :: Map String Board
namedBoards =
    M.fromList [("glider", glider), ("blinker", blinker), ("toad", toad)]

getNamedBoard :: String -> Maybe Board
getNamedBoard = flip M.lookup namedBoards

-- TODO: parseBoard :: Parser Board for command line input of initial board

-- TODO: Make axis/origin positions clear
-- TODO: Dynamic resizing to cover all living cells maybe?
displayBoard :: Bounds -> Board -> String
displayBoard bounds board =
    let displayCell cell = if S.member cell board then '■' else ' '
        displayRow y =
                [ displayCell (x, y) | x <- [minX bounds .. maxX bounds] ]
        grid = map displayRow [minY bounds .. maxY bounds]
    in  displayGrid '|' '-' grid

extrasperse :: a -> [a] -> [a]
extrasperse x xs = x : intersperse x xs ++ [x]

displayGrid :: Char -> Char -> [String] -> String
displayGrid colSep rowSep grid =
    let width      = length . head $ grid
        rowLine    = replicate (width * 2 + 1) rowSep
        displayRow = extrasperse colSep
    in  unlines . extrasperse rowLine . map displayRow $ grid

translateBoard :: Int -> Int -> Board -> Board
translateBoard dx dy = S.map $ \(x, y) -> (x + dx, y + dy)

-- TODO: Parametrize options in some reader monad
displayGame :: Bounds -> Board -> IO ()
displayGame bounds board =
    let game        = playGame board
        screens     = map (putStrLn . displayBoard bounds) game
        title       = "<Press enter to exit>"
        frameTimeMS = 500
        run (screen : rest) = do
            clearScreen
            setCursorPosition 0 0
            putStrLn title
            putStrLn ""
            screen
            pressed <- hWaitForInput stdin frameTimeMS
            unless pressed $ run rest
    in  do
            hideCursor
            run screens
            showCursor
