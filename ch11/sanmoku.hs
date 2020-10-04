import Data.Char
import Data.List
import System.IO
import qualified System.Random as R
import Data.Ord (comparing)

size :: Int 
size = 3

type Grid = [[Player]]

data Player = O | B | X
                deriving (Show, Eq, Ord)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/=B) . concat

turn :: Grid -> Player
turn grid = if os <= xs then O else X
            where
                os = length (filter (== O) ps)
                xs = length (filter (== X) ps)
                ps = concat grid

wins :: Player -> Grid -> Bool
wins player grid = any line (rows ++ cols ++ dias)
                    where
                        line = all (== player)
                        rows = grid
                        cols = transpose grid
                        dias = [diag grid, diag (map reverse grid)]

diag :: Grid -> [Player]
diag grid = [grid !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won grid = wins O grid || wins X grid

putGrid :: Grid -> IO ()
putGrid = 
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid grid idx = 0 <= idx && idx < size^2 && concat grid !! idx == B

move :: Grid -> Int -> Player -> [Grid]
move grid idx player = 
    if valid grid idx then [chop size (xs ++ [player] ++ ys)] else []
    where
        -- 選べるのが、Bの時だからBを抜かし、playerで置き換える
        (xs, B:ys) = splitAt idx (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else
        do 
            putStrLn "ERROR: InvalidNumber"
            getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run grid player = do
    cls
    goto (1,1)
    putGrid (grid)
    run' grid player

run' :: Grid -> Player -> IO ()
run' grid player | wins O grid = putStrLn "Player O wins!\n"
                 | wins X grid = putStrLn "Player X wins!\n"
                 | full grid   = putStrLn "It's a draw!\n"
                 | otherwise   = 
                     do
                         idx <- getNat (prompt player)
                         case move grid idx player of
                            [] -> do 
                                putStrLn "ERROR: Invalid move"
                                run' grid player
                            [grid'] -> run grid' (next player)

prompt :: Player -> String
prompt player = "Player " ++ show player ++ ", enter your move:"


cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int , Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


data Tree a = Node a [Tree a]
              deriving Show

gameTree :: Grid -> Player -> Tree Grid
gameTree grid player = Node grid [gameTree g' (next player) | g' <- moves grid player]

moves :: Grid -> Player -> [Grid]
moves grid player
    | won grid = []
    | full grid = []
    | otherwise = concat [move grid idx player | idx <- [0..((size^2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node grid [])
    | wins O grid = Node (grid, O) []
    | wins X grid = Node (grid, X) []
    | otherwise = Node (grid, B) []
minimax (Node grid ts)
    | turn grid == O = Node (grid, minimum ps) ts'
    | turn grid == X = Node (grid, maximum ps) ts'
        where
            ts' = map minimax ts
            ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove grid player = head [grid' | Node (grid', player') _ <- ts, player' == best]
                       where
                           tree = prune depth (gameTree grid player)
                           Node (_, best) ts = minimax tree

{--
play :: Grid  -> Player -> IO ()
play grid player = do
    cls
    goto (1,1)
    putGrid grid
    play' grid player

play' :: Grid -> Player -> IO ()
play' grid player
    | wins O grid = putStrLn "Player O wins!\n"
    | wins X grid = putStrLn "Player X wins!\n"
    | full grid   = putStrLn "It's a draw!\n"
    | player == O = do
        idx <- getNat (prompt player)
        case move grid idx player of
            [] -> do
                putStrLn "ERROR: Invalid move"
                play' grid player
            [grid'] -> play grid' (next player)
    | player == X = do
        putStrLn "Player X is thinking"
        (play $! (bestmove grid player)) $ next player


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play empty O
--}

--1--
countNode :: Tree Grid -> Int
countNode (Node _ ts) = 1 + sum [countNode t | t <- ts]

countDepth :: Tree Grid -> Int
countDepth (Node _ []) = 0
countDepth (Node _ ts) = maximum [countDepth t | t <- ts] + 1


main :: IO ()
main = do
    --print $ countNode $ prune depth $ gameTree empty O
    print $ minimumDepth' $ gameTree [[O,X,O],[B,B,B],[B,B,B]] X


--2--
bestmove' :: Grid -> Player -> [Grid]
bestmove' grid player = [grid' | Node (grid', player') _ <- ts, player' == best]
                       where
                           tree = prune depth (gameTree grid player)
                           Node (_, best) ts = minimax tree

play'' :: Grid -> Player -> IO ()
play'' grid player
    | wins O grid = putStrLn "Player O wins!\n"
    | wins X grid = putStrLn "Player X wins!\n"
    | full grid   = putStrLn "It's a draw!\n"
    | player == O = do
        idx <- getNat (prompt player)
        case move grid idx player of
            [] -> do
                putStrLn "ERROR: Invalid move"
                play' grid player
            [grid'] -> play grid' (next player)
    | player == X = do
        putStrLn "Player X is thinking"
        let bestmoves = bestmove' grid player
        idx <- R.randomRIO (0, (length bestmoves) - 1)
        play (bestmoves !! idx) $ next player

--3--
{--
bestmove'' :: Grid -> Player -> Grid
bestmove'' grid player = grid'
                       where
                           gametree = gameTree grid player
                           tree = prune depth gametree
                           Node (_, best) ts = minimax tree
                           Node (grid', _) _ = snd $ head $ sortBy (comparing fst) $ map (\t -> (minimumDepth t, t)) $ filter (\(Node (_, player') _) -> player' == best) ts
--}

minimumDepth :: Tree (Grid, Player) -> Int
minimumDepth (Node _ []) = 0
minimumDepth (Node _ ts) = minimum [minimumDepth t | t <- ts] + 1          


bestmove'' :: Grid -> Player -> Grid
bestmove'' grid player = head [grid' | Node (grid', player') _ <- ts, player' == best]
                       where
                           gametree = gameTree grid player
                           minDepth = minimumDepth' gametree
                           tree = prune minDepth (gameTree grid player)
                           Node (_, best) ts = minimax tree


minimumDepth' :: Tree Grid -> Int
minimumDepth' (Node _ []) = 0
minimumDepth' (Node _ ts) = minimum [minimumDepth' t | t <- ts] + 1     

play :: Grid  -> Player -> IO ()
play grid player = do
    cls
    goto (1,1)
    putGrid grid
    play' grid player

play' :: Grid -> Player -> IO ()
play' grid player
    | wins O grid = putStrLn "Player O wins!\n"
    | wins X grid = putStrLn "Player X wins!\n"
    | full grid   = putStrLn "It's a draw!\n"
    | player == O = do
        idx <- getNat (prompt player)
        case move grid idx player of
            [] -> do
                putStrLn "ERROR: Invalid move"
                play' grid player
            [grid'] -> play grid' (next player)
    | player == X = do
        putStrLn "Player X is thinking"
        (play $! (bestmove'' grid player)) $ next player

{--
main = do
    hSetBuffering stdout NoBuffering
    play empty O   
    --}