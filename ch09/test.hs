data Op = Add | Sub | Div | Mul

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Div = "/"
    show Mul = "*"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid Mul _ _ = True

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Div x y = x `div` y
apply Mul x y = x * y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App op l r) = brak l ++ show op ++ brak r
                            where
                                brak (Val n) = show n
                                brak e = "(" ++ show e ++ ")"
    
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op l r) = [apply op x y | x <- eval l,
                                    y <- eval r,
                                    valid op x y]

-- 部分集合 --
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                    yss = subs xs

-- 第一引数を第二引数のリストの各インデックスに挿入したリストを返す --
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- 順列計算 --
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Div, Mul]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = 
    [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = 
    [e | ns' <- choices ns, (e, m) <- results ns', m == n]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Div x y = y /= 1 && x `mod` y == 0
valid' Mul x y = x /= 1 && y /= 1 && x <= y

results'' :: [Int] -> [Result]
results'' [] = []
results'' [n] = [(Val n, n) | n > 0]
results'' ns = [res | (ls, rs) <- split ns,
                    lx <- results'' ls,
                    ry <- results'' rs,
                    res <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) = 
    [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = 
    [e | ns' <- choices ns, (e, m) <- results'' ns', m == n]

{--
main :: IO ()
main = print $ solutions'' [1,3,4,10,25,50] 765
--}


-------------------solution-------------------
--1--
choices' :: [a] -> [[a]]
choices' xs = [xs' | ys <- subs xs, xs' <- perms ys]

--2--
removeValue :: Eq a => a -> [a] -> [a]
removeValue _ [] = []
removeValue x (y:ys) | x == y       = ys
                     | otherwise    = y : removeValue x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice xs (y:ys) = isChoice (removeValue y xs) ys

--3--
{--
空リストを許可すると、リストの長さが減らないので処理が終わらない
split [1,2] -> [([1],[2]), ([1,2], [])]なので、無限に初期リストと同じリストをsplitする
--}

--4--
{-
main :: IO ()
main = do
    let exprList = concat $ map exprs $ choices [1,3,7,10,25,50]
    print $ length exprList
    print $ length $ concat $ map eval exprList
--}

--5--
valid'' :: Op -> Int -> Int -> Bool
valid'' Add x y = True
valid'' Sub _ _ = True
-- 負の数を許可すると演算の結果、0が出る
valid'' Div x y = y /= 0 &&  x `mod` y == 0
valid'' Mul x y = True

eval'' :: Expr -> [Int]
eval'' (Val n) = [n | n /= 0]
eval'' (App op l r) = [apply op x y | x <- eval'' l,
                                      y <- eval'' r,
                                      valid'' op x y]

main :: IO ()
main = do
    let exprList = concat $ map exprs $ choices [1,3,7,10,25,50]
    print $ length exprList
    print $ length $ concat $ map eval'' exprList