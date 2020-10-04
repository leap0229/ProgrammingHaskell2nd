-- 1 --
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving (Show)

instance Functor Tree where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 2 --
{--
instance Functor ((->) a) where
    --fmap (b -> c) -> (a -> b) -> (a -> c)
    fmap f g = f . g
--}

-- 3 --
{--
instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    pure x = const x

    -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x) 
--}

-- 4 --
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z $ map g xs

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z [x | _ <- [0..]]

    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

--5--
-- pure id <*> x = x
-- (a -> f a) (a -> a) f a = f a
-- f (a -> a)  <*> f a = f a
-- f a = f a

-- pure (g x) = pure g <*> pure x
-- (a -> f a) ((a -> b) a) = (a -> f a) (a -> b) <*> (a -> f a) a
-- (a -> f a) (b) = f (a -> b) <*> f a
-- f b = f b

-- x <*> pure y = pure (\g -> g y) <*> x
-- f (a -> b) <*> (a -> f a) a = (a -> f a) ((a -> b) -> b) <*> f (a -> b)
-- f (a -> b) <*> f a = (a -> f a) ((a -> b) -> b) <*> f (a -> b)
-- f b = (a -> f a) ((a -> b) -> b) <*> f (a -> b)
-- f b = f ((a -> b) -> b) <*> f (a -> b)
-- f b = f b

-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- f (b -> c) <*> (f (a -> b) <*> f (a)) = (d -> f d) ((b -> c) -> (a -> b) -> a -> c) <*> f (b -> c) <*> f (a -> b)) <*> f a
-- f (b -> c) <*> f b = f ((b -> c) -> (a -> b) -> a -> c) <*> f (b -> c) <*> f (a -> b)) <*> f a
-- f c = f ((a -> b) -> a -> c) <*> f (a -> b) <*> f a
-- f c = f (a -> c) <*> f a
-- f c = f c

-- 6 --
--instance Monad ((->) a) where
    -- return :: b -> (a -> b)
    -- return = pure

    -- (>>=) :: (a -> c) -> (c -> a -> b) -> (a -> b)
    -- g (>>=) h = \x -> h (g x) x
