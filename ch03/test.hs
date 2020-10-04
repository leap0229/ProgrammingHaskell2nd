bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2], [3, 4]]

add' :: Int -> Int -> Int -> Int
add' x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

{--
second :: [a] -> a
swap :: (a, b) -> (b, a)
pair :: a -> b -> (a, b)
double :: Num a => a -> a
palindrome :: Eq a => [a] -> Bool
twice :: (a -> a) -> a -> a
--}