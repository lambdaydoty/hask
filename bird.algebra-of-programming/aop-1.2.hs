-- 1.2 Natural Numbers

data Nat = Zero | Succ Nat
  deriving Show

m :: Nat
m = Zero

plus :: (Nat,Nat) -> Nat
plus (m,Zero)   = m
plus (m,Succ n) = Succ (plus (m,n))

mult :: (Nat,Nat) -> Nat
mult (m,Zero)   = Zero
mult (m,Succ n) = plus (m,mult(m,n))

zero :: Nat
zero = Zero

one :: Nat
one = Succ Zero

two :: Nat
two = plus (one,one)

data NatF a = ZeroF | SuccF a

fib :: NatF (Int, Int) -> (Int, Int)
fib ZeroF          = (1, 1)
fib (SuccF (m, n)) = (n, n + m)

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

main = do
  print m
  print (plus ((Succ (Zero)), (Succ (Succ Zero)))) -- 1 + 2
  print (mult (two,two))
  print ((cata fib) 1)
