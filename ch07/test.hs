import Data.List
import Data.Char

test :: (a -> Bool) -> (a -> b) -> [a] -> [b]
test p f xs = map f $ filter p xs

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and $ map p xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or $ map p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p allx@(x:xs) | p x = dropWhile' p xs
                    | otherwise = allx

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> (f x) : ys) [] xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr filter'' [] xs
    where
        filter'' x ys | p x = x : ys
                      | otherwise = ys

dec2int :: [Int] -> Int
dec2int xs = foldl (\y x -> x + 10 * y) 0 xs

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = (\x -> (\y -> f (x, y)))

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = (\(a, b) -> f a b)

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x      = []
               |otherwise = h x : unfold p h t (t x)
               
type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (== []) (take 9) (drop 9)

map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f xs = unfold (== []) (f . head) (tail) xs

iterate'' :: (a -> a) -> a -> [a]
iterate'' f xs = unfold (\_ -> False) f f xs

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

addParityBit :: [Bit] -> [Bit]
addParityBit bits | odd $ sum bits = 1 : bits
                  | otherwise = 0 : bits

decode :: [Bit] -> String
decode bits | all verifyParityBit chopped9 = map (chr . bin2int . tail) $ chopped9
            | otherwise = error "parity error"
    where
        chopped9 = chop9 bits
        verifyParityBit (parity:bits) = sum bits `mod` 2 == parity

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

decode' :: [Bit] -> String
decode' = map (chr . bin2int) . chop8

encode' :: String -> [Bit]
encode' = concat . map (make8 . int2bin . ord)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x | doubleNumber > 9 = doubleNumber - 9
             | otherwise        = doubleNumber
    where
        doubleNumber = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = sumNum `mod` 10 == 0
    where
        sumNum = (luhnDouble w) + x + (luhnDouble y) + z

luhn' :: [Int] -> Bool
luhn' xs = sumNum `mod` 10 == 0
    where
        sumNum = sum $ altMap luhnDouble id xs