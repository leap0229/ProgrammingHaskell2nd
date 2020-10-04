data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equivalent Prop Prop
        
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Equivalent (Var 'A') (Not (Var 'A'))

type Assoc k v = [(k, v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x)   = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equivalent p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equivalent p q) = vars p ++ vars q

type Bit = Int

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x      = []
               |otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
            where
                range = [0..(2^n) - 1]
                make n bs = take n (bs ++ repeat 0)
                conv 0 = False
                conv 1 = True

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]



data Expr = Val Int 
          | Add Expr Expr
          | Mult Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

type Cont = [Op]
data Op = EVAL Expr
        | EVALMULT Expr
        | ADD Int
        | MULT Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)
eval' (Mult x y) c = eval' x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (EVALMULT y : c) n = eval' y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval' e []

testMultExpr = Mult (Add (Val 2) (Val 3)) (Val 4)

data Nat = Zero
         | Succ Nat
         deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
        deriving (Show)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
                        EQ -> True 
                        LT -> occurs' x l
                        GT -> occurs' x r

data Tree' a = Leaf' a
             | Node' (Tree' a) (Tree' a)
             deriving (Show)

countLeaf :: Tree' a -> Int
countLeaf (Leaf' _) = 1
countLeaf (Node' l r) = countLeaf l + countLeaf r

testTree :: Tree' Int
testTree = Node' (Node' (Node' (Leaf' 6) (Node' (Leaf' 6) (Leaf' 9))) (Leaf' 4))
                 (Node' (Leaf' 6) (Leaf' 9))

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) | abs (countLeaf l - countLeaf r) <= 1 = balanced l && balanced r
                     | otherwise = False

halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs)
    where
        mid = length xs `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance left) (balance right)
    where
        (left, right) = halve xs 

testExpr = (Add (Add (Val 2) (Val 3)) (Val 4))

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval'' :: Expr -> Int
eval'' expr = folde (+0) (+) expr 

size :: Expr -> Int
size expr = folde (^0) (+) expr

