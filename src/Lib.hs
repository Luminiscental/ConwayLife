module Lib
    ( Board(..)
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
    )
where

import           Data.Set                       ( Set )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( guard
                                                , unless
                                                )
import           System.Console.ANSI            ( clearFromCursorToScreenBeginning
                                                , hideCursor
                                                , showCursor
                                                , setCursorPosition
                                                )
import           System.IO                      ( stdin
                                                , hWaitForInput
                                                )

type Board = Set (Int, Int)

gridRange :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
gridRange ((minX, minY), (maxX, maxY)) = do
    x <- [minX .. maxX]
    y <- [minY .. maxY]
    return (x, y)

neighbourGrid :: (Int, Int) -> [(Int, Int)]
neighbourGrid (x, y) = do
    (x', y') <- gridRange ((x - 1, y - 1), (x + 1, y + 1))
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
    let ((minX, minY), (maxX, maxY)) = getBounds board
        cellsToConsider              = S.fromList
            $ gridRange ((minX - 1, minY - 1), (maxX + 1, maxY + 1))
    in  S.filter (survives board) cellsToConsider

playGame :: Board -> [Board]
playGame board = board : playGame (stepBoard board)

getBounds :: Board -> ((Int, Int), (Int, Int))
getBounds board =
    let cellXs = S.map fst board
        cellYs = S.map snd board
        minX   = fromMaybe 0 $ S.lookupMin cellXs
        maxX   = fromMaybe 0 $ S.lookupMax cellXs
        minY   = fromMaybe 0 $ S.lookupMin cellYs
        maxY   = fromMaybe 0 $ S.lookupMax cellYs
    in  ((minX, minY), (maxX, maxY))

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

-- TODO: Make translation/axis positions clear
displayBoard :: Board -> String
displayBoard board =
    let ((minX, minY), (maxX, maxY)) = getBounds board
        displayCell cell = if S.member cell board then '■' else ' '
        displayRow y = [ displayCell (x, y) | x <- [minX .. maxX] ]
    in  unlines $ map displayRow [minY .. maxY]

displayGame :: Board -> IO ()
displayGame board =
    let game    = playGame board
        screens = map (putStrLn . displayBoard) game
        run (screen : rest) = do
            clearFromCursorToScreenBeginning
            setCursorPosition 0 0
            let title = "Press enter to exit"
            putStrLn title
            putStrLn $ replicate (length title) '='
            screen
            pressed <- hWaitForInput stdin 1000
            unless pressed $ run rest
    in  do
            hideCursor
            run screens
            showCursor
