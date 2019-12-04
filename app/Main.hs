module Main where

import           Lib
import           Options                        ( Options
                                                , defineOptions
                                                , runCommand
                                                , simpleOption
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Control.Applicative            ( liftA3 )
import           System.Exit                    ( die )

data BoardSpec = FromText String | FromFile String | FromName String

-- TODO: Options for board dimension, frametime, display characters
newtype GameOptions = GameOptions { boardSpec :: Maybe BoardSpec }

createOptions :: (Maybe String, Maybe String, Maybe String) -> GameOptions
createOptions (boardText, boardFile, boardName) =
    case
            mapMaybe
                sequence
                [ (FromText, boardText)
                , (FromFile, boardFile)
                , (FromName, boardName)
                ]
        of
            []                     -> GameOptions Nothing
            (boardMaker, text) : _ -> GameOptions . Just $ boardMaker text

instance Options GameOptions where
    defineOptions =
        let
            boardText = simpleOption
                "board"
                Nothing
                (  "A text representation of the board state to start with: "
                ++ "\".\" characters represent dead cells, "
                ++ "\"*\" characters represent live cells, "
                ++ "\",\" characters separate rows"
                )
            boardFile = simpleOption
                "file"
                Nothing
                "A file containing a text representation of the board state to start with"
            boardName = simpleOption
                "name"
                Nothing
                ("A name of the board state to start with, possible names are: "
                ++ show boardNames
                )
        in
            createOptions
                <$> uncurr3 (liftA3 (,,)) (boardText, boardFile, boardName)
        where uncurr3 f (x, y, z) = f x y z

parseBoardIO :: String -> String -> IO (Maybe Board)
parseBoardIO sourceName text = case parseBoard sourceName text of
    Left  err   -> print err >> return Nothing
    Right board -> return (Just board)

getBoard :: BoardSpec -> IO (Maybe Board)
getBoard (FromName name) = case getNamedBoard name of
    Nothing    -> putStrLn ("Unknown board " ++ show name) >> return Nothing
    Just board -> return (Just board)
getBoard (FromFile filename) = do
    text <- readFile filename
    parseBoardIO filename text
getBoard (FromText text) = parseBoardIO "<command-line argument>" text

main :: IO ()
main = runCommand $ \options args -> case boardSpec options of
    Nothing   -> die "For help try --help"
    Just spec -> maybe (die "Could not create board") playBoard
        =<< getBoard spec
      where
        playBoard =
            animateGame $ Bounds { minX = -10, maxX = 10, minY = -5, maxY = 5 }
