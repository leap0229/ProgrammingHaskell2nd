--1--
{--
instance (Monoid a, Monoid b) => Monoid (a, b) where
    -- mempty :: (a, b)
    mempty = (mempty, mempty)

    -- mappend :: (a, b) -> (a, b) -> (a, b)
    (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)
--}

--2--
{--
instance Monoid b => Monoid (a -> b) where
    -- mempty :: (a -> b)
    mempty = (\_ -> mempty)

    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = (\x -> f x `mappend` g x)
--}

--3--
{--
instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    --fold = foldMap id

    -- foldMap :: Monoid a => (a -> b) -> Maybe a -> b
    foldMap f = foldr (mappend . f) mempty

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl _ z Nothing = z
    foldl f z (Just x) = f z x
--}

{--
    instance Traversable Maybe where
    --traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)

    traverse g Nothing = pure Nothing
    traverse g (Just x) = fmap (Just) (g x)
--}

--4--
data Tree a = Leaf | Node (Tree a) a (Tree a)
                deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g Leaf = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ z Leaf = z
    foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l 

    -- foldMap :: Monoid a => (a -> b) -> Tree a -> b
    foldMap f Leaf = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
    --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g Leaf = pure Leaf
    traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r 

--5--
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (filterF')
    where 
        filterF' x | p x = [x]
                   | otherwise = mempty