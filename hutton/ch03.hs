-- ex 1.

-- ex 2.
bools :: [Bool]
bools = [True,True,False,False]

nums :: [[Int]]
nums = [[1],[2,3],[0,-1]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

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

main = do
  print bools
  print nums
  print (add 1 10 100)
  print (copy "hello")
  print ((apply length) [1,2,3])
  print (second ['x', 'y', 'z'])
  print (double 3.14)
  print (palindrome [1,2,3,2,1])
  print (twice double 3.14)
