module Main where

import World
import Board
import AI
import Control.Monad
import Text.Read

go :: IO ()
go = main

main :: IO ()
main = do
    displayInfo

    putStrLn "Play 1 player or 2 player? (inb4 1 or 2)"
    variant <- getInt

    case variant of
        2 -> twoPlayerLoop braveNewWorld
        1 -> onePlayerLoop braveNewWorld

displayInfo :: IO ()
displayInfo = do
    putStrLn "\n----- Lemme explein game -----"
    putStrLn "U wr8 coords and i put ur mark on da board"
    putStrLn "U win after u strike 5 star (but no mor) in da row (or diagonal)"
    putStrLn "Ur mv is evaluated liek this: giv row num (>= 1), next giv col num (>= 1)"
    putStrLn "----- Hev fun -----\n"

twoPlayerLoop :: World -> IO ()
twoPlayerLoop currentWorld@(World board currentPlayer) = do
    putStrLn ""
    putStrLn $ show board
    putStr "----- Its turn for "
    putStr $ show currentPlayer
    putStrLn " -----"

    move <- getMove board

    let newWorld = registerMove currentWorld move
    let end = gameEnded (_board newWorld) move

    unless (end) (twoPlayerLoop newWorld)
    when (end) (finish newWorld)

onePlayerLoop :: World -> IO ()
onePlayerLoop currentWorld@(World board currentPlayer) = do
    case currentPlayer of
        X -> do
            putStrLn ""
            putStrLn $ show board
            move <- getMove board

            let newWorld = registerMove currentWorld move
            let end = gameEnded (_board newWorld) move

            unless (end) (onePlayerLoop newWorld)
            when (end) (finish newWorld)

        O -> do
            let move = makeDecision board

            let newWorld = registerMove currentWorld move
            let end = gameEnded (_board newWorld) move

            unless (end) (onePlayerLoop newWorld)
            when (end) (finish newWorld)

finish :: World -> IO ()
finish currentWorld@(World board currentPlayer) = do
    putStrLn ""
    putStrLn $ show board

    putStrLn "----- Is end -----"
    putStr "Gz to "
    putStrLn $ show $ nextPlayer currentPlayer
    putStrLn "Bye"
    putStrLn ""

getMove :: Board -> IO Position
getMove board = do
    putStrLn "Gimme ur mv"
    x <- getInt
    y <- getInt
    let move = normalisePosition (x, y)
    case move of 
        Valid _ -> do
            case getField board move of
                Just _ -> putStrLn "Bad, alrdy teken, wr8 agen" >> getMove board
                Nothing -> return move
        Invalid -> putStrLn "Bad, outside, wr8 agen" >> getMove board
    

getInt :: IO Int
getInt = do
    line <- getLine
    case readMaybe line of 
        Just a -> return a
        Nothing -> putStrLn "Bad, wr8 agen" >> getInt

normalisePosition :: (Int, Int) -> Position
normalisePosition (x, y) = position (x - 1, y - 1)