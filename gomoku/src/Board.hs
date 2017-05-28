module Board where

import Data.Maybe

type Position = (Int, Int)

data Field = O | X deriving (Eq, Show)
data Col = Col [Maybe Field]
data Board = Board [Col]

showPosition :: Maybe Field -> String
showPosition a = case a of
                    Nothing -> "-"
                    Just a -> show a

instance Show Col where
    show (Col a) = foldl (\x y -> x ++ showPosition y ++ " ") "" a

instance Show Board where
    show (Board a) = foldl (\x y -> x ++ show y ++ "\n") "" a

emptyCol :: Col
emptyCol = Col (replicate 19 Nothing)

emptyBoard :: Board
emptyBoard = Board (replicate 19 $ emptyCol)

modifyColAtPos :: Col -> Position -> Field -> Col
modifyColAtPos (Col fields) y f = emptyCol

putOnBoard :: Board -> Position -> Field -> Board
putOnBoard (Board cols) (x, y) f = emptyBoard