halve :: [a] -> ([a], [a])
halve xs | even $ length xs = (take midIdx xs, drop midIdx xs) 
         | otherwise = ([], [])
    where
        midIdx = length xs `div` 2

third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_ : _ : third : _) = third

safetail :: [a] -> [a]
safetail xs = if null xs
              then []
              else tail xs

safetail' :: [a] -> [a]
safetail' xs | not $ null xs = tail xs
             | otherwise     = []

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(&&) :: Bool -> Bool -> Bool
b1 && b2 = if b1 == True
            then if b2 == True
             then True
            else False
           else False

{--
(&&) :: Bool -> Bool -> Bool
b1 && b2 = if b1 == True
            then b2
           else False
--}

mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x * y * z)))

luhnDouble :: Int -> Int
luhnDouble x | doubleNumber > 9 = doubleNumber - 9
             | otherwise        = doubleNumber
    where
        doubleNumber = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = sumNum `mod` 10 == 0
    where
        sumNum = (luhnDouble w) + x + (luhnDouble y) + z