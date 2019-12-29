{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Lib
Description : The business library for the game.
Maintainer  : luminiscental01@gmail.com

The main class defined is the 'Board' class representing a static state of the game,
with 'animateGame' allowing its evoluation over time to be visualized on a command line.
-}

module Lib
    ( Board
    , Bounds(..)
    , Parser
    , neighbourGrid
    , countNeighbours
    , aliveNext
    , stepBoard
    , generations
    , getBounds
    , displayBoard
    , animateGame
    , emptyBoard
    , parseBoard
    , getNamedBoard
    , boardNames
    , translateBoard
    )
where

import           Text.Parsec                    ( Parsec
                                                , ParseError
                                                , runParser
                                                , many
                                                , sepBy
                                                )
import           Text.Parsec.Char               ( char )
import           Data.Set                       ( Set )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( intersperse )
import           Data.Tuple                     ( swap )
import           Control.Monad                  ( guard
                                                , unless
                                                )
import           Control.Applicative            ( (<|>) )
import           System.Console.ANSI            ( clearScreen
                                                , hideCursor
                                                , showCursor
                                                , setCursorPosition
                                                , getTerminalSize
                                                )
import           System.IO                      ( stdin
                                                , hWaitForInput
                                                )

type Parser = Parsec String ()

-- | The 'Board' type represents a static board for the game of life.
-- It is a set of active cells represented by their cartesian coordinates.
type Board = Set (Int, Int)

-- | The 'Bounds' type represents a rectangular region of the grid.
data Bounds = Bounds { minX :: Int -- ^ The minimum x coordinate of the grid (inclusive).
                     , minY :: Int -- ^ The minimum y coordinate of the grid (inclusive).
                     , maxX :: Int -- ^ The maximum x coordinate of the grid (inclusive).
                     , maxY :: Int -- ^ The maximum y coordinate of the grid (inclusive).
                     }

-- | Extend a region of the grid by growing in every direction.
extendBounds :: Bounds -> Bounds
extendBounds b = Bounds { minX = minX b - 1
                        , minY = minY b - 1
                        , maxX = maxX b + 1
                        , maxY = maxY b + 1
                        }

-- | Create a region containing a single point.
unitBounds :: (Int, Int) -> Bounds
unitBounds (x, y) = Bounds { minX = x, minY = y, maxX = x, maxY = y }

-- | Get a list of all points in a region.
gridRange :: Bounds -> [(Int, Int)]
gridRange b = do
    x <- [minX b .. maxX b]
    y <- [minY b .. maxY b]
    return (x, y)

-- | Get a list of points neighbour to a given point (not including the original point).
neighbourGrid :: (Int, Int) -> [(Int, Int)]
neighbourGrid (x, y) = do
    (x', y') <- gridRange . extendBounds . unitBounds $ (x, y)
    guard (x' /= x || y' /= y)
    return (x', y')

-- | Count the number of living neighbours to a cell in the board.
countNeighbours :: Board -> (Int, Int) -> Int
countNeighbours board position = length liveIndicators
  where
    liveIndicators = filter id indicatorList
    indicatorList  = map (`S.member` board) grid
    grid           = neighbourGrid position

-- | Check whether a cell in the board will be alive next turn.
aliveNext :: Board -> (Int, Int) -> Bool
aliveNext board position =
    let neighbours = countNeighbours board position
        alive      = S.member position board
    in  (neighbours == 3) || (alive && neighbours == 2)

-- | Calculate the next generation of a board.
stepBoard :: Board -> Board
stepBoard board =
    let cellsToConsider =
                S.fromList . gridRange . extendBounds . getBounds $ board
    in  S.filter (aliveNext board) cellsToConsider

-- | Given an initial board state returns an infinite list of the following generations.
generations :: Board -> [Board]
generations board = board : generations (stepBoard board)

-- | Returns the smallest region containing all living cells in the board,
-- or just the origin for an empty board.
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

-- | Create a board with no live cells.
emptyBoard :: Board
emptyBoard = S.empty

-- | A map of board presets which have special names.
namedBoards :: Map String Board
namedBoards =
    M.fromList [("glider", glider), ("blinker", blinker), ("toad", toad)]

-- | Get a board preset from its name.
getNamedBoard :: String -> Maybe Board
getNamedBoard = flip M.lookup namedBoards

-- | A list of names for boards that are supported.
boardNames :: [String]
boardNames = M.keys namedBoards

boardFromGrid :: [[Bool]] -> Board
boardFromGrid rows = S.fromList $ do
    (row , y) <- zip rows [0 ..]
    (cell, x) <- zip row [0 ..]
    guard cell
    return (x, y)

parseBoard
    :: String -- ^ source name
    -> String -- ^ source text
    -> Either ParseError Board
parseBoard sourceName sourceText =
    let cell  = (char '.' >> return False) <|> (char '*' >> return True)
        board = boardFromGrid <$> many cell `sepBy` char ','
    in  runParser board () sourceName sourceText

-- | Display the cells of the board in a given region as a string.
displayBoard :: Bounds -> Board -> String
displayBoard bounds board =
    let displayCell cell = if S.member cell board then '■' else ' '
        displayRow y =
                [ displayCell (x, y) | x <- [minX bounds .. maxX bounds] ]
        grid = map displayRow [minY bounds .. maxY bounds]
    in  displayGrid '|' '-' grid

-- | Easiest described by example:
--
-- >>> extrasperse '|' "123abc"
-- "|1|2|3|a|b|c|"
extrasperse :: a -> [a] -> [a]
extrasperse x xs = x : intersperse x xs ++ [x]

displayGrid :: Char -> Char -> [String] -> String
displayGrid colSep rowSep grid =
    let width      = length . head $ grid
        rowLine    = replicate (width * 2 + 1) rowSep
        displayRow = extrasperse colSep
    in  unlines . extrasperse rowLine . map displayRow $ grid

-- | Shift every cell of the board by a given change in x and y.
translateBoard :: Int -> Int -> Board -> Board
translateBoard dx dy = S.map $ \(x, y) -> (x + dx, y + dy)

conservativeHalve :: Int -> Int
conservativeHalve n = (n - 1) `div` 2

-- | Create a centered bounding region from given dimensions.
-- If a dimension is even the resulting bounds will have the next smallest
-- corresponding odd dimension.
centeredBounds :: (Int, Int) -> Bounds
centeredBounds (width, height) =
    let halfW = conservativeHalve width
        halfH = conservativeHalve height
    in  Bounds { minX = -halfW, minY = -halfH, maxX = halfW, maxY = halfH }

-- | Adjust terminal dimensions to board dimensions to account for row/column separators.
adjustDimensions :: (Int, Int) -> (Int, Int)
adjustDimensions (x, y) = (conservativeHalve x, conservativeHalve y - 2)

-- TODO: Allow input to translate the view
-- TODO: Parametrize options
-- | Animate the generations of a given initial board state on the command line,
-- with the option to exit by pressing enter.
animateGame
    :: Bounds -- ^ The default bounds to use if terminal size can't be found from 'getTerminalSize'.
    -> Board -- ^ The initial board state to animate
    -> IO ()
animateGame defaultBounds board = do
    terminalSize <- getTerminalSize
    let bounds = maybe defaultBounds
                       (centeredBounds . adjustDimensions . swap)
                       terminalSize
        game        = generations board
        screens     = map (putStrLn . displayBoard bounds) game
        title       = "<Press enter to exit>"
        frameTimeMS = 500
        resetScreen = clearScreen >> setCursorPosition 0 0
        run []              = return ()
        run (screen : rest) = do
            resetScreen
            putStrLn title
            putStrLn ""
            _       <- screen
            pressed <- hWaitForInput stdin frameTimeMS
            unless pressed $ run rest
    hideCursor
    run screens
    resetScreen
    showCursor
