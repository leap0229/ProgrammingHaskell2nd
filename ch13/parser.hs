import Control.Applicative ( Alternative(..) )
import Data.Char
    ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )
import System.IO (stdin, hSetEcho)

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Paser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp ->  case parse pg inp of
                                []      -> []
                                [(g, out)] -> parse (fmap g px) out)

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> parse (f v) out)

three' :: Parser (Char, Char)
three' = do
            x <- item
            item
            z <- item
            return (x, z)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
            x <- item
            if p x then
                return x
            else
                empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                    char x
                    string xs
                    return (x:xs)

ident :: Parser String
ident = do
            x <- lower
            xs <- many alphanum
            return (x:xs)

nat :: Parser Int 
nat = do
        xs <- some digit
        return (read xs)

space :: Parser ()
space = do
            many (sat isSpace)
            return ()

int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
        <|> nat

token :: Parser a -> Parser a
token p = do
            space
            v <- p
            space
            return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
            symbol "["
            n <- natural
            ns <- many (do
                            symbol ","
                            natural)
            symbol "]"
            return (n:ns)

expr :: Parser Int
expr = do
            t <- term
            do
                symbol "+"
                e <- expr
                return (t + e)
              <|> return t

term :: Parser Int
term = do
            f <- factor
            do
                symbol "*"
                t <- term
                return (f * t)
              <|> return f

factor :: Parser Int
factor = do
            symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "Invald input"

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int , Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
                goto p
                putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"
       ]

buttons :: String
buttons = standard ++ extra
            where
                standard = "qcd=123+456-789*0()/"
                extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]

display :: String -> IO ()
display xs = do
                writeat (3, 2) (replicate 13 ' ')
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
            display xs
            c <- getCh
            if elem c buttons then
                process c xs
            else do 
                    beep
                    calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"  = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval' xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case (parse expr xs) of
             [(n, [])] -> calc $ show n
             -- 9 --
             [(_, xs')] -> do 
                            calc xs'
             _          -> do 
                            beep
                            calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
        cls
        showbox
        clear

--1--
comment :: Parser ()
comment = do
            symbol "--"
            many (sat (/= '\n'))
            return ()

--2,3,4--
--略

--5--

--6--
expr' :: Parser Int
expr' = do
            t <- term'
            do
                symbol "+"
                e <- expr'
                return (t + e)
                <|> do
                        symbol "-"
                        e <- expr'
                        return (t - e) 
              <|> return t

term' :: Parser Int
term' = do
            f <- factor'
            do
                symbol "*"
                t <- term'
                return (f * t)
                <|> do
                        symbol "/"
                        t <- term'
                        return (f `div` t)
              <|> return f

factor' :: Parser Int
factor' = do
            symbol "("
            e <- expr'
            symbol ")"
            return e
          <|> integer

eval'' :: String -> Int
eval'' xs = case (parse expr' xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "Invald input"

--7--
{--
expr ::= term (+expr | epsilon)
term ::= factor (*term | epsilon)
factor ::= atom (^factor | epsilon)
atom ::= (expr) | nat
--}

factor'' :: Parser Int
factor'' = do
            a <- atom
            do
                symbol "^"
                f <- factor''
                return (a ^ f)
                <|> return a

atom :: Parser Int
atom = do
        symbol "("
        e <- expr'
        symbol ")"
        return e
      <|> integer

--8--
--a--
-- expr ::= expr - nat | nat
-- nat ::= natural

--b--
exprLeft :: Parser Int
exprLeft = do
            e <- exprLeft
            do
                symbol "-"
                n <- natural
                return (e - n)
                <|> natural

exprLeft' :: Parser Int
exprLeft' = do
                n <- natural
                ns <- many (do
                                symbol "-"
                                natural)
                return (foldl (-) n ns)

--5--
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

expr'' :: Parser Expr
expr'' = do
            t <- term
            do
                symbol "+"
                e <- expr''
                return (Add (Val t) e)
              <|> return (Val t)