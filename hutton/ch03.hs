{-


  -------------------
   f e :: B
-}

-- ex 3.
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x + x

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- ex 4.

-- ex 5.

putNewLn () = putStrLn "\n"

main = do
  putStrLn "{- 3.1 Basic concepts -}"
  putStrLn ""
  putStrLn " f :: A -> B "
  putStrLn " e :: A "
  putStrLn "-------------"
  putStrLn " f e :: B "
  putStrLn ""
  putStrLn " Every expression must have a type, "
  putStrLn " which is calculated prior to evaluating the expression "
  putStrLn " by a process called `type inference` "
  putNewLn ()
  --
  putStrLn "{- 3.2 Basic types -}"
  putStrLn (show (True :: Bool))
  putStrLn (show (False :: Bool))
  putStrLn (show ('a' :: Char))
  putStrLn (show ('A' :: Char))
  putStrLn (show ('3' :: Char))
  putStrLn (show ('_' :: Char))
  putStrLn (show ('\n' :: Char))
  putStrLn (show ('\t' :: Char))
  putStrLn (show ("abc" :: String))
  putStrLn (show ("1+2=3" :: String))
  putStrLn (show ("" :: String))
  putStrLn (show (-100 :: Int)) -- Fixed-precision integers
  putStrLn (show (0 :: Int))
  putStrLn (show (999 :: Int))
  putStrLn (show (2^63 :: Int))
  putStrLn (show (2^63 :: Integer)) -- Arbitrary-precision integers
  putStrLn (show (-12.34 :: Float)) -- Single-precision floating-point numbers
  putStrLn (show (1.0 :: Float))
  putStrLn (show (3.1415927 :: Float))
  putStrLn (show (sqrt 2 :: Float))
  putStrLn (show (sqrt 99999 :: Float))
  putStrLn (show (sqrt 2 :: Double)) -- Double-precision floating-point numbers
  putNewLn ()
  --
  putStrLn "{- 3.3 List types [T] -}"
  putStrLn (show ([False,True,False] :: [Bool]))
  putStrLn (show (['a','b','c','d'] :: [Char]))
  putStrLn (show (["One","Two","Three"] :: [String]))
  putStrLn (show ([['a','b'],['c','d','e']] :: [[Char]]))
  putNewLn ()
  --
  putStrLn "{- 3.4 Tuple types (T1,T2,...,Tn) -}"
  putStrLn (show ((False,True) :: (Bool,Bool)))
  putStrLn (show ((False,'a',True) :: (Bool,Char,Bool)))
  putStrLn (show (("Yes",True,'a') :: (String,Bool,Char)))
  putStrLn ("Arity = 0 : () ... empty tuple")
  putStrLn ("Arity = 2 : (1,2) ... pair")
  putStrLn ("Arity = 3 : (1,2,3) ... triple")
  putStrLn (show (('a',(False,'b')) :: (Char,(Bool,Char))))
  putStrLn (show ((['a','b'],[False,True]) :: ([Char],[Bool])))
  putStrLn (show ([('a',False),('b',True)] :: [(Char,Bool)]))
  putNewLn ()
  --
  putStrLn "{- 3.5 Function types -}"
  putNewLn ()
  --
  putStrLn "{- 3.6 Curried functions -}"
  putStrLn " -> is associated to the RIGHT: "
  putStrLn " Int -> Int -> Int -> Int === Int -> (Int -> (Int -> Int)) "
  putStrLn " `function application` is associated to the LEFT: "
  putStrLn " mult x y z === ((mult x) y) z "
  putNewLn ()
  --
  putStrLn "{- 3.7 Polymorphic types -}"
  putStrLn "length :: [a] -> Int"
  putStrLn "fst :: (a,b) -> a"
  putStrLn "head :: [a] -> a"
  putStrLn "take :: Int -> [a] -> [a]"
  putStrLn "zip :: [a] -> [b] -> [(a,b)]"
  putStrLn "id ::  a -> a"
  putNewLn ()
  --
  putStrLn "{- 3.8 Overloaded types -}"
  putStrLn "(+) :: Num a => a -> a -> a"
  putStrLn "(*) :: Num a => a -> a -> a"
  putStrLn "negate :: Num a => a -> a"
  putStrLn "abs :: Num a => a -> a"
  putStrLn (show (3 :: Num a => a))
  putNewLn ()
  --
  putStrLn "{- 3.9 Basic classes -}"
  putStrLn ""
  putStrLn " A `type` is a collection of related values. "
  putStrLn " A `class` is a collection of types that support certain overloaded `methods` "
  putStrLn ""
  putStrLn "Eq a"
  putStrLn "(==), (/=) :: a -> a -> Bool"
  putStrLn ""
  putStrLn "Ord a"
  putStrLn "(<), (<=), (>), (>=) :: a -> a -> Bool"
  putStrLn "min, max :: a -> a -> a"
  putStrLn ""
  putStrLn "Show a"
  putStrLn "show :: a -> String"
  putStrLn ""
  putStrLn "Read a"
  putStrLn "read :: String -> a"
  putStrLn ""
  putStrLn "Num a"
  putStrLn "(+), (-), (*) :: a -> a -> a"
  putStrLn "negate, abs, signum :: a -> a"
  putStrLn ""
  putStrLn "Integral a => Num a"
  putStrLn "div, mod :: a -> a -> a"
  putStrLn ""
  putStrLn "Fractional a => Num a"
  putStrLn "(/) :: a -> a -> a"
  putStrLn "recip :: a -> a"
  putNewLn ()
  --
  putStrLn "{- ex 3.1 -}"
  putStrLn (show (['a','b','c'] :: [Char]))
  putStrLn (show (('a','b','c') :: (Char,Char,Char)))
  putStrLn (show ([(False,'0'),(True,'1')] :: [(Bool,Char)]))
  putNewLn ()
  --
  putStrLn "{- ex 3.2 -}"
  putStrLn (show ([True,True] :: [Bool]))
  putStrLn (show ([[1],[2,3]] :: [[Int]]))
  putStrLn (show (
    ((\x -> \y -> \z -> x) :: Int -> Int -> Int -> Int) 3 4 5
                 ))
  putNewLn ()
  --
  putStrLn "{- ex 3.3 -}"
  putStrLn "{- ex 3.4 -}"
  putStrLn (show (((head . tail) :: [x] -> x) [1,2])) -- second
  putStrLn (show (((\(x,y) -> (y,x)) :: (a,b) -> (b,a)) (1,"x"))) -- swap
  putStrLn (show (((\x -> \y -> (x,y)) :: a -> b -> (a,b)) 1 "x")) -- pair
  putStrLn (show (((\x -> x * 2) :: Num a => a -> a) 3)) -- double
  putStrLn (show (((\xs -> reverse xs == xs) :: Eq a => [a] -> Bool) [1,2,3,2,1])) -- palindrome
  putStrLn (show (((\f -> \x -> f (f x)) :: (a -> a) -> a -> a) not True)) -- twice
  putNewLn ()
  --
  putStrLn "{- ex 3.5 -}"
  putStrLn ("?")
  putNewLn ()
  --
