-- ex 1.
mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p xs = map f (filter p xs)

-- ex 2.
all2 :: (a -> Bool) -> [a] -> Bool
all2 p = and . map p

any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x:xs)
  | p x = x : takeWhile2 p xs
  | otherwise = []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x:xs)
  | p x = dropWhile2 p xs
  | otherwise = x : xs

-- ex 3.

-- (x1 # (x2 # (x3 # (... # e))))
-- op   : #
-- id   : e
-- list : [x1, x2, ...]
foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ e []     = e
foldr0 o e (x:xs) = o x (foldr0 o e xs)

-- fx1 : fx2 : fx3 : ...
map0 :: (a -> b) -> [a] -> [b]
map0 f = foldr0 (\x -> \y -> f x : y) []

-- fx1 : fx2 : fx3 : ...
filter0 :: (a -> Bool) -> [a] -> [a]
filter0 p = foldr0 (\x -> \y -> if p x then x : y else y) []

-- ex 4.

-- (((...(e # ...) # x3) # x2) # x1)
-- op   : #
-- id   : e
-- list : [..., x3, x2, x1]
foldl0 :: (b -> a -> b) -> b -> [a] -> b
foldl0 o e xs = fn e xs
  where fn y []     = y
        fn y (z:zs) = fn (o y z) zs

dec2int :: [Int] -> Int
dec2int = foldl0 (\x y -> 10 * x + y) 0

-- ex 5.
curry5 :: ((a,b) -> c) -> a -> b -> c
curry5 f = \x y -> f (x,y)

uncurry5 :: (a -> b -> c) -> (a,b) -> c
uncurry5 f = \(x,y) -> f x y

-- ex 6. -- 展開
-- p : predicate
-- h : head-applying
-- t : transformer
-- x : argument
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map6 :: (a -> b) -> [a] -> [b]
map6 f = unfold null (f . head) tail

iterate6 :: (a -> a) -> a -> [a]
iterate6 t = unfold (\_ -> False) (\x -> x) t

-- ex 7.
-- TODO

-- ex 8.
-- TODO

-- ex 9.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g [x]      = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- ex 10.
-- TODO

main = do
  print (takeWhile2 (\x -> x > 0) [1,-1,2,-2,3,-3,0])
  print (dropWhile2 (\x -> x > 0) [1,-1,2,-2,3,-3,0])
  print (map0 (\x -> x * x) [1, 2, 3, 4, 5])
  print (filter0 (\x -> x > 0) [1,-1,2,-2,3,-3,0])
  print (foldl0 (+) 0 [1,-1,2,-2,3,-3,0])
  print (dec2int [2,3,4,5])
  print (int2bin 65534)
  print (chop8 (int2bin 65534))
  print (map6 (\x -> x * x) [1, 2, 3, 4, 5])
  print (take 10 (iterate6 (2*) 1))
  print (altMap (+10) (+100) [0,1,2,3,4])
