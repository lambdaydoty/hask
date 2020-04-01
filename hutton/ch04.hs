-- ex 1.
halve :: [a] -> ([a],[a])
halve = \xs -> splitAt ((length xs) `div` 2) xs

-- ex 2.
thirda :: [a] -> a
thirda = \xs -> head (tail (tail xs))

thirdb :: [a] -> a
thirdb = \xs -> xs !! 2

thirdc :: [a] -> a
thirdc (_:(_:(x:_))) = x

-- ex 3.
safetaila :: [a] -> [a]
safetaila = \xs -> if null xs then [] else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs   = []
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc []     = []
safetailc (_:xs) = xs

-- ex 4.
disjunction1 :: Bool -> Bool -> Bool
True `disjunction1` True   = True
True `disjunction1` False  = True
False `disjunction1` True  = True
False `disjunction1` False = False

disjunction2 :: Bool -> Bool -> Bool
False `disjunction2` False = False
_     `disjunction2` _     = True

disjunction3 :: Bool -> Bool -> Bool
True  `disjunction3` _ = True
False `disjunction3` x = x

disjunction4 :: Bool -> Bool -> Bool
b `disjunction4` c | b == c    = b
                   | otherwise = True

-- ex 5.
and5 :: Bool -> Bool -> Bool
and5 = \b -> \c -> if b then if c then True else False else False

-- ex 6.
and6 :: Bool -> Bool -> Bool
and6 = \b -> \c -> if b then c else False

-- ex 7.
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- ex 8.
luhnDouble :: Int -> Int
luhnDouble n = if m > 9 then m - 9 else m
  where m = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z u = ((sum [luhnDouble x, y, luhnDouble z, u]) `mod` 10) == 0

main = do
  print (halve [1, 2, 3, 4, 5, 6])
  print (thirda ['x', 'y', 'z', 's', 't', 'u'])
  print (thirdb ['x', 'y', 'z', 's', 't', 'u'])
  print (thirdc ['x', 'y', 'z', 's', 't', 'u'])
  print (tail [1, 2, 3, 4, 5, 6])
  print (safetaila [1, 2, 3, 4, 5, 6])
  print ((safetaila []) :: [Int])
  print (safetailb [1, 2, 3, 4, 5, 6])
  print ((safetailb []) :: [Int])
  print (safetailc [1, 2, 3, 4, 5, 6])
  print ((safetailc []) :: [Int])
  print (True `disjunction1` False)
  print (True `disjunction2` False)
  print (True `disjunction3` False)
  print (True `disjunction4` False)
  print (True  `and5` True)
  print (True  `and5` False)
  print (False `and5` True)
  print (False `and5` False)
  print (True  `and6` True)
  print (True  `and6` False)
  print (False `and6` True)
  print (False `and6` False)
  print (mult 1 2 3)
  print (luhnDouble 3)
  print (luhnDouble 6)
  print (luhn 1 7 8 4)
  print (luhn 4 7 8 3)
