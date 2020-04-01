-- ex 1.
fact :: Int -> Int
fact n | n < 0 = 0
       | n == 0 = 1
       | otherwise = n * fact (n - 1)

-- ex 2.
sumdown :: Int -> Int
sumdown 1 = 1
sumdown n = n + sumdown (n - 1)

-- ex 3.
caret :: Int -> Int -> Int
caret _ 0 = 1
caret x n = x * (x `caret` (n - 1))

-- ex 4.
euclid :: Int -> Int -> Int
euclid m n | m > n = euclid (m - n) n
           | m < n = euclid m (n - m)
           | otherwise = m

-- ex 5.
-- TODO

-- ex 6.
and6 :: [Bool] -> Bool
and6 []     = True
and6 (x:xs) = x && and xs

concat6 :: [[a]] -> [a]
concat6 []       = []
concat6 (xs:xss) = xs ++ concat6 xss

replicate6 :: Int -> a -> [a]
replicate6 0 x = []
replicate6 n x = x : replicate6 (n - 1) x

select6 :: [a] -> Int -> a
select6 (x:xs) 0 = x
select6 (x:xs) n = select6 xs (n - 1)

elem6 :: Eq a => a -> [a] -> Bool
elem6 _ []     = False
elem6 x (y:xs) = x == y || elem6 x xs

-- ex 7.
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs         = xs
merge xs []         = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- ex 8.
msort :: Ord a => [a] -> [a]
halve :: [a] -> ([a],[a])

halve xs = (take m xs, drop m xs)
  where m = length xs `div` 2

msort [] = []
msort [x] = [x]
msort xs = merge (msort halve1) (msort halve2)
  where (halve1, halve2) = halve xs

-- ex 9.

-- ex 10.

main = do
  print (fact (-1))
  print (sumdown 3)
  print (2 `caret` 3)
  print (euclid 6 27)
  print (and6 [True, True, True, False])
  print (concat6 [[1,2,3], [4,5], [6,7]])
  print (replicate6 5 'a')
  print (select6 "hello, world!" 7)
  print (elem6 '!' "hello, world!")
  print (merge [2,5,6] [1,3,4])
  print (merge [1,2,3] [10,20,30])
  print (halve [1,2,3,4])
  print (msort [0,9,2,6,7,2,7,2,1,2])
