module Main where

import World
import Board
import Control.Monad
import Text.Read

main :: IO ()
main = do
    putStrLn "Lemme explein game"
    putStrLn "U wr8 coords and i put ur mark on da board"
    putStrLn "U win after u strike 5 star (but no mor) in da row (or diagonal)"
    putStrLn "Ur mv is evaluated liek this: giv row num (>= 1), next giv col num (>= 1)"
    putStrLn "Hev fun"
    loop braveNewWorld

loop :: World -> IO ()
loop currentWorld@(World board currentPlayer) = do
    putStrLn $ show board

    move <- getMove currentWorld

    unless (2 < 1) (loop $ registerMove currentWorld (normalisePosition move))

getMove :: World -> IO (Int, Int)
getMove currentWorld@(World board _) = do
    putStrLn "Gimme ur mv"
    x <- getCoord
    y <- getCoord
    let move = (x, y)
    case getField board (normalisePosition (x, y)) of
        Just a -> putStrLn "Bad, alrdy teken, wr8 agen" >> getMove currentWorld
        Nothing -> return (x, y)

getCoord :: IO Int
getCoord = do
    line <- getLine
    case readMaybe line of 
        Just a -> return a
        Nothing -> putStrLn "Bad, wr8 agen" >> getCoord

normalisePosition :: Position -> Position
normalisePosition (x, y) = (x - 1, y - 1)