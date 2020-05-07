-- 1.1 Functions
double x = x + x

-- ex 3.
prod []     = 1
prod (n:ns) = n * prod ns

sum0 :: Num a => [a] -> a
sum0 []     = 0
sum0 (n:ns) = n + sum0 ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]


{- Sequencing actions -}
seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x:xs)
-- seq :: Monad m => [m a] -> m [a]

product0 :: Num a => [a] -> a
product0 []     = 1
product0 (x:xs) = x * product0 xs

reverseQsort :: Ord a => [a] -> [a]
reverseQsort [] = []
reverseQsort (x:xs) = reverseQsort larger ++ [x] ++ reverseQsort smaller
  where
    larger  = [y | y <- xs, y >= x]
    smaller = [y | y <- xs, y < x]

uniqQsort :: Ord a => [a] -> [a]
uniqQsort [] = []
uniqQsort (x:xs) = uniqQsort smaller ++ [x] ++ uniqQsort larger
  where
    smaller = [y | y <- xs, y < x]
    larger  = [y | y <- xs, y > x]

--
--

putNewLn () = putStrLn "\n"

main = do
  putStrLn "{- 1.1 Functions -}"
  putStrLn ("(double 3) = " ++ show(double 3))
  putStrLn ("(double (double 2)) = " ++ show(double (double 2)))
  putNewLn ()
  --
  putStrLn "{- 1.2 Functional programming -}"
  putStrLn ("sum [1..5] = " ++ show(sum [1..5]))
  putNewLn ()
  --
  putStrLn "{- 1.5 A taste of Haskell -}"
  putStrLn ("sum0 [1,2,3] = " ++ show(sum0 [1,2,3]))
  putStrLn ("qsort [3,5,1,4,2] = " ++ show(qsort [3,5,1,4,2]))
  putStr "seqn [getChar,getChar,getChar]..."
  xs <- seqn [getChar,getChar,getChar]
  putNewLn ()
  --
  putStrLn "{- ex 1.1 -}"
  putStrLn "double (double 2)"
  putStrLn "= double (2 + 2)"
  putStrLn "= (2 + 2) + (2 + 2)"
  putStrLn "= 4 + (2 + 2)"
  putStrLn "= 4 + 4"
  putStrLn "= 8"
  putNewLn ()
  --
  putStrLn "{- ex 1.2 -}"
  putStrLn "sum0 [x]"
  putStrLn "= x + sum0 []"
  putStrLn "= x + 0"
  putStrLn "= x"
  putNewLn ()
  --
  putStrLn "{- ex 1.3 -}"
  putStrLn ("product0 [2, 3, 4] = " ++ show (product0 [2,3,4]))
  putNewLn ()
  --
  putStrLn "{- ex 1.4 -}"
  putStrLn ("reverseQsort [0,9,2,6,7,2,7,2,1,2] = " ++ show (reverseQsort [0,9,2,6,7,2,7,2,1,2]))
  putNewLn ()
  --
  putStrLn "{- ex 1.5 -}"
  putStrLn ("uniqQsort [2,2,3,1,1] = " ++ show (uniqQsort [2,2,3,1,1]))
  putNewLn ()
