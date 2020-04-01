{-# LANGUAGE TemplateHaskell #-}
import           Data.Char

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
                          (Var 'A') (Var 'B'))) (Var 'B')
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq a => a -> [(a,b)] -> b
find x assoc = head [y | (x',y) <- assoc, x' == x]

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = (eval s p) && (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdumps :: Eq a => [a] -> [a]
rmdumps []     = []
rmdumps (x:xs) = x : filter (/= x) (rmdumps xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdumps (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

main = do
  print (isTaut p1)
  print (isTaut p2)
  print (isTaut p3)
  print (isTaut p4)
