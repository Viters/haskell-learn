module AI where

import World
import Board
import Data.List
import Data.Maybe

data GameTree = GameTree World Position [GameTree] | Empty

calcAtt :: Board -> Int -> Int -> Int -> Position -> (Int, Int) -> Bool -> Int
calcAtt board n currVal currMet currPos move@(x, y) turnFlag
    | n < 5 && isValidPos (movePosition currPos move) == False && turnFlag == False =
        calcAtt board n currVal currMet (movePosition currPos (-1 * n * x, -1 * n * y)) (-1 * x, -1 * y) True
    | n < 5 && isValidPos (movePosition currPos move) == False && turnFlag == True =
        currVal
    | n < 5 && turnFlag == False && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just X) =
        calcAtt board (n + 1) currVal currMet (movePosition currPos (-1 * n * x, -1 * n * y)) (-1 * x, -1 * y) True
    | n < 5 && turnFlag == True && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just X) =
        0
    | n < 5 && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Nothing) =
        calcAtt board (n + 1) (currVal + 1) currMet (movePosition currPos move) move turnFlag
    | n < 5 && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just O) =
        calcAtt board (n + 1) (currVal + (currMet * 10)) (currMet * 10) (movePosition currPos move) move turnFlag
    | n == 5 = currVal

calcAttForPos :: Board -> Position -> (Int, Int) -> Int
calcAttForPos board pos@(Valid _) move = calcAtt board 0 0 1 pos move False
calcAttForPos _ Invalid _ = -1

calcAttInEveryWay :: Board -> Position -> Int
calcAttInEveryWay board pos@(Valid _)
    | isJust (getField board pos) = -1
    | otherwise = sum $ map (\x -> calcAttForPos board pos x) $ delete (0, 0) [(x,y) | x<-[-1, 0, 1], y<-[-1, 0, 1]]
calcAttInEveryWay board Invalid = -1

calcAttForBoard :: Board -> [((Int, Int), Int)]
calcAttForBoard board = map (\(x, y) -> ((x, y), (calcAttInEveryWay board (position (x, y))))) [(x,y) | x <- [0..3], y <- [0..3]]

calcDef :: Board -> Int -> Int -> Int -> Position -> (Int, Int) -> Bool -> Int
calcDef board n currVal currMet currPos move@(x, y) turnFlag
    | n < 5 && isValidPos (movePosition currPos move) == False && turnFlag == False =
        calcDef board n currVal currMet (movePosition currPos (-1 * n * x, -1 * n * y)) (-1 * x, -1 * y) True
    | n < 5 && isValidPos (movePosition currPos move) == False && turnFlag == True =
        currVal
    | n < 5 && turnFlag == False && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just O) =
        calcDef board (n + 1) currVal currMet (movePosition currPos (-1 * n * x, -1 * n * y)) (-1 * x, -1 * y) True
    | n < 5 && turnFlag == True && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just O) =
        0
    | n < 5 && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Nothing) =
        calcDef board (n + 1) (currVal + 1) currMet (movePosition currPos move) move turnFlag
    | n < 5 && isValidPos (movePosition currPos move) && (getField board (movePosition currPos move) == Just X) =
        calcDef board (n + 1) (currVal + (currMet * 5)) (currMet * 5) (movePosition currPos move) move turnFlag
    | n == 5 = currVal

calcDefForPos :: Board -> Position -> (Int, Int) -> Int
calcDefForPos board pos@(Valid _) move = calcDef board 0 0 1 pos move False
calcDefForPos _ Invalid _ = -1

calcDefInEveryWay :: Board -> Position -> Int
calcDefInEveryWay board pos@(Valid _)
    | isJust (getField board pos) = -1
    | otherwise = sum $ map (\x -> calcDefForPos board pos x) $ delete (0, 0) [(x,y) | x<-[-1, 0, 1], y<-[-1, 0, 1]]
calcDefInEveryWay board Invalid = -1

calcDefForBoard :: Board -> [((Int, Int), Int)]
calcDefForBoard board = map (\(x, y) -> ((x, y), (calcDefInEveryWay board (position (x, y))))) [(x,y) | x <- [0..3], y <- [0..3]]

calcScore :: Board -> [((Int, Int), Int)]
calcScore board = zipWith (\(x, a) (y, b) -> (x, a + b)) (calcAttForBoard board) (calcAttForBoard board)

testBoard = Board [ (Row [Just O, Nothing, Nothing, Just X]), (Row [Nothing, Nothing, Just X, Just O]), (Row $ replicate 4 Nothing), (Row [Just O, Just O, Nothing, Nothing]) ]
