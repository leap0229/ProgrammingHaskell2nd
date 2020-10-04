double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last' xs = head $ drop (length xs - 1) xs
last'' xs = head $ reverse xs 

init' xs = take (length xs - 1) xs 
init'' xs = reverse $ tail $ reverse xs