module World where

import Board

data CurrentPlayer = P1 | P2 deriving (Eq, Show)
data World = World Board CurrentPlayer deriving Show

braveNewWorld :: World
braveNewWorld = World emptyBoard P1

passTurn :: World -> World
passTurn (World a p) = case p of
                            P1 -> (World a P2)
                            P2 -> (World a P1)