--1--
import System.IO (stdin, hSetEcho)
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar c | c <- xs]

--2, 3--
--see nim.hs

--4--
adder :: IO ()
adder = do
    putStr "How many numbers?"
    number <- getLine
    total <- readNumbers 0 $ read number
    putStrLn $ "The total is " ++ show total


readNumbers :: Int -> Int -> IO Int
readNumbers total 0 = return total
readNumbers total number = do
    n <- getLine
    readNumbers (total + (read n)) (number - 1)

-- 5--
adder' :: IO ()
adder' = do
    putStr "How many numbers?"
    number <- getLine
    inputNumbers <- sequence [getLine | _ <- [1..read number]]
    putStrLn $ "The total is " ++ show (sum $ map read inputNumbers)

--6--
readLine :: IO String
readLine = readLine' ""
    
readLine' :: String -> IO String
readLine' xs = do
    x <- getCh 
    case x of
        '\n' -> do
            putStrLn ""
            return xs
        '\DEL' -> 
            if null xs then
                readLine' xs
            else
                do
                    putStr "\b \b"
                    readLine' $ init xs
        _ -> do
            putChar x
            readLine' (xs ++ [x])

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x