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

gameEnded :: Board -> Position -> Bool
gameEnded board lastMove = 
    or $ map (\x -> isWinningCombination board lastMove x) [fiveInRow, fiveInCol, fiveInDiagLeftRight, fiveInDiagRightLeft]

isWinningCombination :: Board -> Position -> (Position -> Int -> [Position]) -> Bool
isWinningCombination board lastMove f = 
    exactlyOneTrue $ map (\x -> allTheSame $ positionsToFields board x) (zipWith (\x y -> x lastMove y) (replicate 7 f) [-3 .. 3])

fiveInRow :: Position -> Int -> [Position]
fiveInRow (a, b) offset = zip (repeat a) [(b - 2 + offset) .. (b + 2 + offset)]

fiveInCol :: Position -> Int -> [Position]
fiveInCol (a, b) offset = zip [(a - 2 + offset) .. (a + 2 + offset)] $ repeat b

fiveInDiagLeftRight :: Position -> Int -> [Position]
fiveInDiagLeftRight (a, b) offset = zip [(a - 2 + offset) .. (a + 2 + offset)] [(b - 2 + offset) .. (b + 2 + offset)]

fiveInDiagRightLeft :: Position -> Int -> [Position]
fiveInDiagRightLeft (a, b) offset = zip [(a - 2 + offset) .. (a + 2 + offset)] (reverse [(b - 2 - offset) .. (b + 2 - offset)])

positionsToFields :: Board -> [Position] -> [Maybe Player]
positionsToFields board = map $ getField board

allTheSame :: [Maybe Player] -> Bool
allTheSame xs = (head xs /= Nothing) && (and $ map (== head xs) (tail xs))

exactlyOneTrue :: [Bool] -> Bool
exactlyOneTrue l = count True l == 1 

count :: Eq a => a -> [a] -> Int
count needle haystack = length (filter (==needle) haystack)