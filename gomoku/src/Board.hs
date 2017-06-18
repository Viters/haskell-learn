{-# LANGUAGE TemplateHaskell #-}

module Board where

import Data.Maybe
import Control.Lens

type Position = (Int, Int)

data Player = O | X deriving (Eq, Show)
data Row = Row { _columns :: [Maybe Player] }
makeLenses ''Row
data Board = Board { _rows :: [Row] }
makeLenses ''Board

showPosition :: Maybe Player -> String
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

putInRow :: Row -> Int -> Player -> Row
putInRow (Row fields) y f = Row $ (element y .~ Just f) fields

putOnBoard :: Board -> Position -> Player -> Board
putOnBoard (Board rows) (x, y) f = Board $ (element x .~ putInRow (rows !! x) y f) rows

getField :: Board -> Position -> Maybe Player
getField board pos@(x, y)
    | isPositionValid pos = (((board^.rows) !! x)^.columns) !! y
    | otherwise = Nothing

isPositionValid :: Position -> Bool
isPositionValid (a, b)
    | a >= 1 && a <= 19 && b >= 1 && b <= 19 = True
    | otherwise = False