import Data.Maybe

data Field = O | X deriving (Eq, Show)
data Row = Row [Maybe Field]
data Board = Board [Row]

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