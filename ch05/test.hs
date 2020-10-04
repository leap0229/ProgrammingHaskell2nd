import Data.Char

squareSum :: Int
squareSum = sum [ x ^ 2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], (sum $ factors x) == 2 * x]

seven = [(x, y)| x <- [1,2,3], y <- [4,5,6]]
seven' = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | i <- find x $ zip xs [0..]]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper $ int2let ((let2int (toLower c) + n) `mod` 26)
          | otherwise = c

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x lowersStr) n | x <- ['a'..'z']]
            where lowersStr = map toLower xs
                  n = lowers $ lowersStr

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs