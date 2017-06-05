data Box a = Box a | Empty

-- class Functor m where
--     fmap :: (a -> b) -> m a -> m b

-- class Applicative m where
--     pure :: a -> m a
--     <*> :: m (a -> b) -> m a -> m b

instance Functor Box where
    fmap f Empty = Empty
    fmap f (Box a) = Box $ f a

instance Applicative Box where
    pure a = Box a
    Empty <*> (Box _) = Empty
    (Box _) <*> Empty = Empty
    (Box f) <*> (Box a) = Box $ f a