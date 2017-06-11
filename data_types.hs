data Tree a = TreeNode a [Tree a] deriving Show

genTree :: a -> Int -> Int -> Tree a
genTree v h c
    | h == 1 = TreeNode v []
    | otherwise = TreeNode v $ replicate c $ genTree v (h - 1) c

instance Functor Tree where
    fmap f (TreeNode a c) = TreeNode (f a) $ map (fmap f) c

data List a = ListNode a (List a) | Empty deriving Show

map' :: (a -> b) -> List a -> List b
map' f (ListNode a l) = ListNode (f a) (map' f l)
map' f Empty = Empty

foldr' :: (b -> a -> a) -> a -> List b -> a
foldr' f a (ListNode v l) = f v (foldr' f a l)
foldr' f a Empty = a

foldl' :: (a -> b -> a) -> a -> List b -> a
foldl' f a (ListNode v l) = foldl' f (f a v) l
foldl' f a Empty = a

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (ListNode a l1) (ListNode b l2) = ListNode (f a b) $ zipWith' f l1 l2
zipWith' f _ Empty = Empty
zipWith' f Empty _ = Empty

concat' :: List a -> List a -> List a
concat' a Empty = a
concat' Empty a = a
concat' (ListNode a l1) l2 = ListNode a $ concat' l1 l2

fromNormalList :: [a] -> List a
fromNormalList (x:xs) = ListNode x $ fromNormalList xs
fromNormalList [] = Empty

toNormalList :: List a -> [a]
toNormalList (ListNode v l) = [v] ++ toNormalList l
toNormalList Empty = []

instance Functor List where
    fmap f l = map' f l

instance Applicative List where
    pure a = ListNode a Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (ListNode f l1) <*> (ListNode b l2) = ListNode (f b) (l1 <*> l2)
    