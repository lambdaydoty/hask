{-
   f :: A -> B
   e :: A
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
  putStrLn "{- 3.2 Basic types -}"
  putStrLn (show (True :: Bool))
  putStrLn (show (False :: Bool))
  putStrLn (show ('c' :: Char))
  putStrLn (show ('\n' :: Char))
  putStrLn (show (2^63 :: Int))
  putStrLn (show (2^63 :: Integer))
  putStrLn (show (sqrt 2 :: Float))
  putStrLn (show (sqrt 2 :: Double))
  putNewLn ()
  putStrLn "{- 3.3 List types -}"
  putStrLn (show (["One","Two","Three"] :: [String]))
  putNewLn ()
  putStrLn "{- 3.4 Tuple types -}"
  putStrLn (show ((False,'a',True) :: (Bool,Char,Bool)))
  putStrLn ("Arity = 0 : () ... empty tuple")
  putStrLn ("Arity = 2 : (1,2) ... pair")
  putStrLn ("Arity = 3 : (1,2,3) ... triple")
  putNewLn ()
  putStrLn "{- 3.5 Function types -}"
  putStrLn "{- 3.6 Curried functions -}"
  putStrLn "{- 3.7 Polymorphic types -}"
  putStrLn "{- 3.8 Overloaded types -}"
  putStrLn "{- 3.9 Basic classes -}"
  putStrLn "Eq a"
  putStrLn "(==), (/=) :: a -> a -> Bool"
  putStrLn "Ord a"
  putStrLn "(<), (<=), (>), (>=) :: a -> a -> Bool"
  putStrLn "min, max :: a -> a -> a"
  putStrLn "Show a"
  putStrLn "show :: a -> String"
  putStrLn "Read a"
  putStrLn "read :: String -> a"
  putStrLn "Num a"
  putStrLn "(+), (-), (*) :: a -> a -> a"
  putStrLn "negate, abs, signum :: a -> a"
  putStrLn "Integral a => Num a"
  putStrLn "div, mod :: a -> a -> a"
  putStrLn "Fractional a => Num a"
  putStrLn "(/) :: a -> a -> a"
  putStrLn "recip :: a -> a"
  putStrLn "{- ex 3.4 -}"
  putStrLn (show (second ['x', 'y', 'z']))
  putStrLn (show (double 3.14))
  putStrLn (show (palindrome [1,2,3,2,1]))
  putStrLn (show (twice double 0.25))
