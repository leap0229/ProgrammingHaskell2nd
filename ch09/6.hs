import Control.Exception (assert)
import Debug.Trace (trace)
data Op = Add | Sub | Div | Mul | Pow

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Div = "/"
    show Mul = "*"
    show Pow = "^"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
-- Pow したときにIntだとオーバーフローして0が入る可能性があるので、y /= 0 をチェック
valid Div x y = y /= 0 && x `mod` y == 0
valid Mul _ _ = True
valid Pow _ y = y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Div x y = x `div` y
apply Mul x y = x * y
apply Pow x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App op l r) = brak l ++ show op ++ brak r
                            where
                                brak (Val n) = show n
                                brak e = "(" ++ show e ++ ")"

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

type Result = (Expr, Int)

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

results'' :: [Int] -> [Result]
results'' [] = []
results'' [n] = [(Val n, n) | n > 0]
results'' ns = [res | (ls, rs) <- split ns,
                    lx <- results'' ls,
                    ry <- results'' rs,
                    res <- combine'' lx ry]

ops :: [Op]
ops = [Add, Sub, Div, Mul, Pow]

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) = 
    [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = 
    [e | ns' <- choices ns, (e, m) <- results'' ns', m == n]

main :: IO ()
main = print $ solutions'' [1,3,5,7,25,50] 765
