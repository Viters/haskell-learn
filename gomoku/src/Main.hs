module Main where

import World
import Board
import Control.Monad
import Text.Read

go :: IO ()
go = main 

main :: IO ()
main = do
    putStrLn "\n----- Lemme explein game -----"
    putStrLn "U wr8 coords and i put ur mark on da board"
    putStrLn "U win after u strike 5 star (but no mor) in da row (or diagonal)"
    putStrLn "Ur mv is evaluated liek this: giv row num (>= 1), next giv col num (>= 1)"
    putStrLn "If u giv move out of map u gonna pass turn"
    putStrLn "----- Hev fun -----\n"
    loop braveNewWorld

loop :: World -> IO ()
loop currentWorld@(World board currentPlayer) = do
    putStrLn $ show board
    putStr "----- Its turn for "
    putStr $ show currentPlayer
    putStrLn " -----"

    move <- getMove board

    let newWorld = registerMove currentWorld move
    let end = gameEnded (_board newWorld) move

    unless (end) (loop newWorld)
    when (end) (finish newWorld)


finish :: World -> IO ()
finish currentWorld@(World board currentPlayer) = do
    putStrLn $ show board

    putStrLn "Is end"
    putStr "Gz to "
    putStrLn $ show $ nextPlayer currentPlayer
    putStrLn "Bye"

getMove :: Board -> IO (Int, Int)
getMove board = do
    putStrLn "Gimme ur mv"
    x <- getCoord
    y <- getCoord
    let move = normalisePosition (x, y)
    case getField board move of
        Just a -> putStrLn "Bad, alrdy teken, wr8 agen" >> getMove board
        Nothing -> return move

getCoord :: IO Int
getCoord = do
    line <- getLine
    case readMaybe line of 
        Just a -> return a
        Nothing -> putStrLn "Bad, wr8 agen" >> getCoord

normalisePosition :: Position -> Position
normalisePosition (x, y) = (x - 1, y - 1)