module Main where

import World
import Control.Monad
import Text.Read

main :: IO ()
main = do
    let world = braveNewWorld
    loop world

loop :: World -> IO ()
loop currentWorld@(World board currentPlayer) = do
    putStrLn $ show board

    putStrLn "Write x and y separated with a newline"
    x <- getCoord
    y <- getCoord
    let move = (x, y)

    unless (2 < 1) (loop $ registerMove currentWorld move)

getCoord :: IO Int
getCoord = do
    line <- getLine
    case readMaybe line of 
        Just a -> return a
        Nothing -> putStrLn "Invalid input, try again" >> getCoord