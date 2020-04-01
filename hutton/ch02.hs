-- ex 1.

-- ex 2.

-- ex 3.
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- ex 4.
lasto xs = xs !! (length xs - 1)
lastRev xs = head (reverse xs)
lastRecur [x] = x
lastRecur (x:xs) = lastRecur xs

-- ex 5.
inito xs = take (length xs - 1) xs
initRev xs = reverse (tail (reverse xs))
initRecur [x] = []
initRecur (x:xs) = [x] ++ initRecur(xs)

main = do
  print n
  print (last [0,9,2,6,7,2,7,2,1,2])
  print (lasto [0,9,2,6,7,2,7,2,1,2])
  print (lastRev [0,9,2,6,7,2,7,2,1,2])
  print (lastRecur [0,9,2,6,7,2,7,2,1,2])
  print (init [0,9,2,6,7,2,7,2,1,2])
  print (inito [0,9,2,6,7,2,7,2,1,2])
  print (initRev [0,9,2,6,7,2,7,2,1,2])
  print (initRecur [0,9,2,6,7,2,7,2,1,2])
