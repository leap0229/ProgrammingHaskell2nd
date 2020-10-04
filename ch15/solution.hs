-- 1 --
{--
1+(2*3)
2*3 inner and outer

(1+2) * (2+3)
1+2 inner and outer
2+3 inner and outer

fst (1+2, 2+3)
fst (,) outer
1+2 inner
2+3 inner

(\x -> 1 + x) (2*3)
2*3 inner
(\x -> 1 + x) (2*3)  outer
--}

{-- 2
第二引数は不要なため、最内簡約の場合、２要素目の評価が無駄
--}

{--3
mult = \x -> (\y -> x * y)
mult 3 4
1. multが適用（multは引数取らないかつ、左のため）
(\x -> (\y -> x * y)) 3 4
2.\x -> (\y -> x * y) 3が評価
(\y -> 3 * y) 4
3.(\y -> 3 * y) 4 が評価
3 * 4
4. 3 * 4 が評価
12
--}

{--4--}
fibs' :: [Integer]
fibs' = 0 : 1 : [x + y | (x, y) <- zip fibs' $ tail fibs']

fibs :: [Integer]
fibs = 0 : 1 : fib [0,1]

fib :: [Integer] -> [Integer]
fib (x:y:_) = x + y : fib [y, x + y]

{--5--}
{--
repeat' :: a -> [a]
repeat' x = xs where xs = x:xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take (n - 1) xs

replicate' :: Int -> a -> [a]
replicate' n = take n . repeat
--}

repeat' :: a -> Tree a
repeat' x = t where t = Node t x t

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' _ Leaf = Leaf
take' n (Node l x r) = Node (take' (n - 1) l) x (take' (n - 1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

{--6--}
sqroot :: Double -> Double
sqroot n = head [new | (old, new) <- zip guesses $ tail guesses, enough old new]
    where next x = (x + n / x) / 2
          guesses = iterate next n
          enough old new = abs (old - new) < 0.00001

guess :: Fractional a => a -> [a]
guess x = iterate (\g -> (g + x/g) / 2.0) 1.0

less_enough :: (Ord a, Fractional a) => a -> a -> a -> Bool
less_enough eps old new = abs (1.0 - old/new) < eps

find_sqrt :: (t -> t -> Bool) -> t -> [t] -> t
find_sqrt f i (x:xs) | f i x        = x
                     | otherwise    = find_sqrt f x xs

sqroot' :: (Ord a, Fractional a) => a -> a
sqroot' n = find_sqrt (less_enough 0.00001) n (guess n)

