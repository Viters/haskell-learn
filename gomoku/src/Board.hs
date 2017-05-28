module Board where

import Data.Maybe
import Control.Lens

type Position = (Int, Int)

data Field = O | X deriving (Eq, Show)
data Row = Row { _columns :: [Maybe Field] }
data Board = Board { _rows :: [Row] }

showPosition :: Maybe Field -> String
showPosition a = case a of
                    Nothing -> "-"
                    Just a -> show a

instance Show Row where
    show (Row a) = foldl (\x y -> x ++ showPosition y ++ " ") "" a

instance Show Board where
    show (Board a) = foldl (\x y -> x ++ show y ++ "\n") "" a

emptyRow :: Row
emptyRow = Row (replicate 19 Nothing)

emptyBoard :: Board
emptyBoard = Board (replicate 19 $ emptyRow)

setInRow :: Row -> Int -> Field -> Row
setInRow (Row fields) y f = Row $ (element y .~ Just f) fields

putOnBoard :: Board -> Position -> Field -> Board
putOnBoard (Board rows) (x, y) f = Board $ (element x .~ setInRow (rows !! x) y f) rows