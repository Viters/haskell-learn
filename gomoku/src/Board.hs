{-# LANGUAGE TemplateHaskell #-}

module Board where

import Data.Maybe
import Control.Lens

data Position = Valid (Int, Int) | Invalid deriving (Eq, Show)
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

emptyBoard :: Board
emptyBoard = Board (replicate 19 $ Row (replicate 19 Nothing))

putInRow :: Row -> Int -> Player -> Row
putInRow (Row fields) y f = Row $ (element y .~ Just f) fields

putOnBoard :: Board -> Position -> Player -> Board
putOnBoard (Board rows) (Valid (x, y)) f = Board $ (element x .~ putInRow (rows !! x) y f) rows

getField :: Board -> Position -> Maybe Player
getField board (Valid (x, y)) = (((board^.rows) !! x)^.columns) !! y
getField board Invalid = Nothing

position :: (Int, Int) -> Position
position (x, y)
    | x >= 0 && x <= 3 && y >= 0 && y <= 3 = Valid (x, y)
    | otherwise = Invalid

movePosition :: Position -> (Int, Int) -> Position
movePosition (Valid (x0, y0)) (x, y) = position (x0 + x, y0 + y)

isValidPos :: Position -> Bool
isValidPos (Valid _) = True
isValidPos Invalid = False