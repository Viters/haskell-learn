import Data.Maybe

data Field = O | X deriving (Eq, Show)
data Position = Maybe Field
data Board = Matrix [[Position]]

showPosition :: Maybe Position -> String
showPosition (Just a) = show a
showPosition (Nothing) = "_"

-- instance Show Board where
--     show (Matrix []) = ""
    -- show (Matrix a) = show a

emptyBoard :: Board
emptyBoard = Matrix (replicate 19 $ replicate 19 Nothing)