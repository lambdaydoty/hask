-- ex 1.
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n     = Zero
mult (Succ m) n = add n (mult m n)

instance Show Nat where
  show Zero     = "0"
  show (Succ m) = "1+" ++ (show m)

-- ex 2.
data Tree a = Leaf a | Node (Tree a) a (Tree a)


occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

-- ex 3.
data BTree a = BLeaf a | BNode (BTree a) (BTree a)

nleaves :: BTree a -> Int
nleaves (BLeaf _)   = 1
nleaves (BNode l r) = nleaves l + nleaves r

balanced :: BTree a -> Bool
balanced (BLeaf _) = True
balanced (BNode l r) = balanced l && balanced r && abs (m - n) <= 1
  where m = nleaves l
        n = nleaves r

-- ex 4.
balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs = BNode (balance (fst sp)) (balance (snd sp))
  where sp = splits xs

splits :: [a] -> ([a],[a])
splits xs = (take m xs, drop m xs)
  where m = length xs `div` 2

-- instance Show (BTree a) where
--   show (BLeaf x)   = show x
--   show (BNode l r) = "(" ++ (show l) ++ " " ++ (show r) ")"

-- ex 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)     = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-- ex 6.
eval :: Expr -> Int
eval = folde (\x -> x) (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- ex 7.
-- instance Eq a => Eq (Maybe a) where
--   Just a == Just b   = a == b
--   Nothing == Nothing = True
--   _ == _             = False

-- instance Eq a => Eq [a] where
--   [] == []         = True
--   (x:xs) == (y:ys) = x == y && xs == ys
--   _ == _           = False

-- ex 8.
-- TODO

-- ex 9.
-- TODO

main = do
  print (Zero)
  print (Succ Zero)
  print (add (Succ Zero) (Succ Zero))
  print (mult (add (Succ Zero) (Succ Zero)) (add (Succ Zero) (Succ Zero)))
  print (eval (Val 3))
  print (eval (Add (Val 1) (Val 1)))
  print (size (Val 3))
  print (size (Add (Val 1) (Val 1)))
