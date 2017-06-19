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
    or $ map (\x -> isWinningCombination board lastMove x) [inRow, inCol, inDiagRL, inDiagLR]

isWinningCombination :: Board -> Position -> (Position -> Int -> [Position]) -> Bool
isWinningCombination board lastMove f = 
    exactlyOneTrue $ map (\x -> allTheSame $ positionsToFields board x) (getPositionsCombinations lastMove f)

getPositionsCombinations :: Position -> (Position -> Int -> [Position]) -> [[Position]]
getPositionsCombinations lastMove f = filter (\x -> length x == 5) (zipWith (\x y -> x lastMove y) (replicate 9 f) [-4 .. 4])

validatePositions :: [(Int, Int)] -> [Position] 
validatePositions xs = filter isValidPos $ map position xs

inRow :: Position -> Int -> [Position]
inRow (Valid (a, b)) offset = validatePositions $ zip (repeat a) (fives b offset)

inCol :: Position -> Int -> [Position]
inCol (Valid (a, b)) offset = validatePositions $ zip (fives a offset) (repeat b)

inDiagRL :: Position -> Int -> [Position]
inDiagRL (Valid (a, b)) offset = validatePositions $ zip (fives (a + offset) 0) (fives (b + offset) 0)

inDiagLR :: Position -> Int -> [Position]
inDiagLR (Valid (a, b)) offset = validatePositions $ zip (fives (a - offset) 0) (reverse $ fives (b + offset) 0)

fives :: Int -> Int -> [Int]
fives mid offset = [(mid - 2 + offset) .. (mid + 2 + offset)]

positionsToFields :: Board -> [Position] -> [Maybe Player]
positionsToFields board = map $ getField board

allTheSame :: [Maybe Player] -> Bool
allTheSame xs = (head xs /= Nothing) && (and $ map (== head xs) (tail xs))

exactlyOneTrue :: [Bool] -> Bool
exactlyOneTrue xs = count True xs == 1 

count :: Eq a => a -> [a] -> Int
count needle haystack = length (filter (==needle) haystack)