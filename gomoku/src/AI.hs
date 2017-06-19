module AI where

import World
import Board

data GameTree = GameTree World Position [GameTree] | Empty

makeDecision :: Board -> Position
makeDecision board = position (1, 1)