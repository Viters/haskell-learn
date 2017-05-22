sum' :: [Integer] -> Integer
sum' = foldr (+) 0

product :: [Integer] -> Integer
product = foldr (*) 1

reverse' :: [a] -> [a]
reverse' = foldl (\acc curr -> curr : acc) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

head' :: [a] -> a
head' = foldr (\curr _ -> curr) undefined

last' :: [a] -> a
last' = foldl (\_ curr -> curr) undefined

map' :: (a -> b) -> [a] -> [b]
map' f x = foldr (\curr acc -> (f curr) : acc) [] x