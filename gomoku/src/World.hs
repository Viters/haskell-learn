module World where

import Board

data World = World { _board :: Board, _currentPlayer :: Player } deriving Show

braveNewWorld :: World
braveNewWorld = World emptyBoard X

nextPlayer :: Player -> Player
nextPlayer a = case a of 
                    X -> O
                    O -> X

registerMove :: World -> Position -> World
registerMove (World board cp) pos = World (putOnBoard board pos cp) (nextPlayer cp)

-- gameEnded :: Board -> Boolean
-- gameEnded = 

fiveInRow :: Position -> [Position]
fiveInRow (a, b) = zip [a-2 .. a+2] $ repeat b

fiveInCol :: Position -> [Position]
fiveInCol (a, b) = zip [a-2 .. a+2] $ repeat b

fiveInDiagLeftRight :: Position -> [Position]
fiveInDiagLeftRight (a, b) = zip [a-2 .. a+2] [b-2 .. b+2]

fiveInDiagRightLeft :: Position -> [Position]
fiveInDiagRightLeft (a, b) = zip [a-2 .. a+2] [b+2 .. b-2]

positionsToFields :: Board -> [Position] -> [Maybe Player]
positionsToFields board = map $ getField board

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)