fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = n
      | otherwise = n * fac (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^) :: Int -> Int ->  Int
(^) 0 _ = 0
(^) _ 0 = 1
(^) n m = n * (n Main.^ (m - 1))

euclid :: Int -> Int -> Int
euclid _ 1 = 1
euclid n m | n == m = n
           | otherwise = euclid minNum $ max n m - minNum
        where
            minNum = (min n m)

and' :: [Bool] -> Bool
and' [] = True
and' (False:_) = False
and' (True:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:ys) = xs ++ concat' ys

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = xs Main.!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allx@(x:xs) ally@(y:ys) | x < y     = x : merge xs ally
                              | otherwise = y : merge allx ys

halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs)
    where
        mid = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort pre) (msort bef)
    where
        (pre, bef) = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs