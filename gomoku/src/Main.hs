module Main where

import World

main :: IO ()
main = do
    world <- braveNewWorld
    -- let world = makeMove world (1, 0)

