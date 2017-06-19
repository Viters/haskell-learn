module AI where

import World
import Board
import System.Random

data GameTree = GameTree World Position [GameTree] | Empty

makeDecision :: Board -> IO Position
makeDecision board = do
    x <- randomRIO (0, 18 :: Int)
    y <- randomRIO (0, 18 :: Int)
    return $ position (x, y)
