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