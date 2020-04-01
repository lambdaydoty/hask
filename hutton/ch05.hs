-- ex 1.
s :: Int
s = sum [x * x | x <- [1..100]]

-- ex 2.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- ex 3.
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- ex 4.
replicate0 :: Int -> a -> [a]
replicate0 n x = [x | _ <- [1..n]]

-- ex 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x * x + y * y == z * z]

-- ex 6.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int  -> [Int]
perfect n = [x | x <- [1..n], sum (factors x) == (x * 2)]

-- ex 7.
answer7 :: [(Int,Int)]
answer7 = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- ex 8.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = (length xs) - 1

-- ex 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [(xs !! i) * (ys !! i) | i <- [0..n]]
  where n = (length xs) - 1

-- ex 10.
-- TODO

main = do
  print s
  print (grid 1 2)
  print (square 2)
  print (replicate0 3 True)
  print (pyths 10)
  print (factors 10)
  print (perfect 500)
  print answer7
  print (positions False [True, False, True, False])
  print (scalarproduct [1,2,3] [4,5,6])

