-- 1.1 Functions
double x = x + x

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (n:ns) = n + sum' ns
-- Note: It is a `fold left`
-- sum [1,2,3]
-- = 1 + sum [2,3]
-- = 1 + (2 + sum [3])
-- = 1 + (2 + (3 + sum []))
-- = 1 + (2 + (3 + 0))
-- = 1 + (2 + 3)
-- = 1 + 5
-- = 6

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
--
-- qsort [x]
-- = qsort [] ++ [x] ++ qsort []
-- = [] ++ [x] ++ []
-- = [x]
--
-- qsort [3,5,1,4,2]
-- = qsort [1,2] ++ [3] ++ qsort [5,4]
-- = (qsort [] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ qsort [])
-- = ([] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ [])
-- = [1,2] ++ [3] ++ [4,5]
-- = [1,2,3,4,5]


{- Sequencing actions -}
-- seq :: Monad m => [m a] -> m [a]
seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x:xs)

-- ex 3.
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- ex 4.
reverseQsort :: Ord a => [a] -> [a]
reverseQsort [] = []
reverseQsort (x:xs) = reverseQsort larger ++ [x] ++ reverseQsort smaller
  where
    larger  = [y | y <- xs, y >= x]
    smaller = [y | y <- xs, y < x]

-- ex 5.
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
  putStrLn "{- 1.3 Features of Haskell -}"
  putStrLn "* Concise programs (2,4)"
  putStrLn "* Powerful type systems (3,8)"
  putStrLn "* List comprehensions (5)"
  putStrLn "* Recursive functions (6)"
  putStrLn "* Higher-order functions (7)"
  putStrLn "* Effectful functions (10,12)"
  putStrLn "* Generic functions (12,14)"
  putStrLn "* Lazy evaluation (15)"
  putStrLn "* Equational reasoning (16,17)"
  --
  putStrLn "{- 1.4 Historical background -}"
  putStrLn "1. 1930s, Alonzo Church: the lambda caluclus"
  putStrLn "2. 1950, John McCarthy: Lisp"
  putStrLn "3. 1960, Peter Landin: ISWIM"
  putStrLn "4. 1970, John Backus: FP / Robin Milner: ML"
  putStrLn "5. 1980, David Tuner: Miranda"
  putStrLn "6. 1990, Philip Wadler: Haskell, Monads"
  --
  putStrLn "{- 1.5 A taste of Haskell -}"
  putStrLn ("sum' [1,2,3] = " ++ show(sum' [1,2,3]))
  putStrLn ("qsort [3,5,1,4,2] = " ++ show(qsort [3,5,1,4,2]))
  putStr "seqn [getChar,getChar,getChar]..."
  xs <- seqn [getChar,getChar,getChar]
  putStrLn (show xs)
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
  putStrLn "sum' [x]"
  putStrLn "= x + sum' []"
  putStrLn "= x + 0"
  putStrLn "= x"
  putNewLn ()
  --
  putStrLn "{- ex 1.3 -}"
  putStrLn ("product' [2, 3, 4] = " ++ show (product' [2,3,4]))
  putNewLn ()
  --
  putStrLn "{- ex 1.4 -}"
  putStrLn ("reverseQsort [0,9,2,6,7,2,7,2,1,2] = " ++ show (reverseQsort [0,9,2,6,7,2,7,2,1,2]))
  putNewLn ()
  --
  putStrLn "{- ex 1.5 -}"
  putStrLn ("uniqQsort [2,2,3,1,1] = " ++ show (uniqQsort [2,2,3,1,1]))
  putNewLn ()
