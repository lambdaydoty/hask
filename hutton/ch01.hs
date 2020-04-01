double x = x + x

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

-- seqn [] = return []
-- seqn (act:acts) = do x <- act
--                      xs <- seqn acts
--                      return (x:xs)

-- ex 1.
-- double (double 2)
-- = double 2 + double 2
-- = (2 + 2) + double 2
-- = (2 + 2) + (2 + 2)
-- = 4 + (2 + 2)
-- = 4 + 4
-- = 8

-- ex 2.
-- sum [x]
-- = x + sum []
-- = x + 0
-- = x

-- ex 3.
prod [] = 1
prod (n:ns) = n * prod ns

-- ex 4.
reverseQsort [] = []
reverseQsort (x:xs) = reverseQsort larger ++ [x] ++ reverseQsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

-- ex 5.
uniqQsort [] = []
uniqQsort (x:xs) = uniqQsort smaller ++ [x] ++ uniqQsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger  = [b | b <- xs, b > x]

main = do
  print (qsort [0,9,2,6,7,2,7,2,1,2])
  print (double 3)
  print (prod [2,3,4])
  print (reverseQsort [0,9,2,6,7,2,7,2,1,2])
  print (uniqQsort [0,9,2,6,7,2,7,2,1,2])
