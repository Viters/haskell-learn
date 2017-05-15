import System.IO

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

empty :: Tree a -> Bool
empty Empty = True
empty _ = False

isBinary :: Ord a => Tree a -> Bool
isBinary Empty = True
isBinary (Node l _ r) = isBinary l && isBinary r

contains :: Ord a => Tree a -> a -> Bool
contains Empty _ = False
contains (Node l v r) x
    |   x == v = True
    |   otherwise = contains l x || contains r x

height :: Tree a -> Int
height Empty = 0
height (Node l _ r) = 1 + max (height l) (height r)

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node l _ r) = 
    isBalanced l && isBalanced r && (abs (height l - height r) < 2)

isLeaf :: Eq a => Tree a -> Bool
isLeaf Empty = False
isLeaf (Node l _ r) = l == Empty && r == Empty

nodeCount :: Tree a -> Int
nodeCount Empty = 0
nodeCount (Node l _ r) = nodeCount l + nodeCount r + 1

nodeSum :: Num a => Tree a -> a
nodeSum Empty = 0
nodeSum (Node l v r) = v + nodeSum l + nodeSum r

tMap :: (a -> b) -> Tree a -> Tree b
tMap _ Empty = Empty
tMap f (Node l v r) = Node (tMap f l) (f v) (tMap f r)

leaves :: Eq a => Tree a -> [a]
leaves Empty = []
leaves node@(Node l v r)
    |   isLeaf node = [v]
    |   otherwise = leaves l ++ leaves r

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node l v r) = True

-- TESTS

testTree = (Node (Node Empty 5 Empty) 7 (Node Empty 11 Empty))
nonBalancedTestTree = (Node (Node (Node (Node Empty 3 Empty) 5 Empty) 6 Empty) 8 Empty)

tests = [
    (empty testTree == False, "testTree shouldn't be empty"),
    (empty Empty == True, "Empty tree should be empty"),
    (contains testTree 5 == True, "testTree should contain 5"),
    (contains testTree 12 == False, "testTree shouldn't contain 12"),
    (isBinary testTree == True, "testTree should be binary"),
    (height testTree == 2, "testTree height should be 2"),
    (height Empty == 0, "Empty tree height should be 0"),
    (isBalanced testTree == True, "testTree should be balanced"),
    (isBalanced nonBalancedTestTree == False, "nonBalancedTestTree shouldn't be balanced"),
    (isLeaf testTree == False, "testTree shouldn't be leaf"),
    (isLeaf nonBalancedTestTree == False, "nonBalancedTestTree shouldn't be leaf"),
    (isLeaf (Node Empty 5 Empty) == True, "Given leaf should be leaf"),
    (nodeCount testTree == 3, "testTree should have 3 nodes"),
    (nodeCount nonBalancedTestTree == 4, "nonBalancedTestTree should have 4 nodes"),
    (nodeSum testTree == 23, "testTree should have total value of 23"),
    (tMap (* 2) testTree == Node (Node Empty 10 Empty) 14 (Node Empty 22 Empty), "testTree mapped with (* 2) should be (Node (Node Empty 10 Empty) 14 (Node Empty 22 Empty))"),
    (leaves testTree == [5,11], "testTree should have 2 leaves: 5 and 11"),
    (leaves nonBalancedTestTree == [3], "nonBalancedTestTree should have 1 leaf: 3"),
    (isBST testTree == True, "testTree should be BST")
    ]

main :: IO()
main = 
    mapM_ (putStrLn . snd) (filter (\(x,_) -> x == False) tests)