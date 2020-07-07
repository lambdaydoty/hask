{- 2.5 Haskell scripts-}
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns


{- ex 2.3 -}
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

{- 3 last implements -}
-- ap ∷ m (a → b) → m a → m b
lastSelect = (!!) <*> ((subtract 1) . length)
lastRevHead = head . reverse
lastRecur (x:[]) = x
lastRecur (x:xs) = lastRecur xs

-- ap ∷ m (a → b) → m a → m b
-- flip ∷ (a → b → c) → b → a → c
-- take ∷ Int → [a] → [a]
initTake = (flip take) <*> ((subtract 1) . length)
initRev = reverse . tail . reverse
initRecur (x:[]) = []
initRecur (x:xs) = x : initRecur(xs)

putNewLn () = putStrLn "\n"

main = do
  putStrLn "{- 2.1 Glasgow Haskell Compiler -}"
  putStrLn "* The compiler: GHC"
  putStrLn "* The interpreter: GHCi"
  putNewLn ()
  --
  putStrLn "{- 2.2 Installing and starting -}"
  putStrLn "$ ghci"
  putStrLn "$ runhaskell source.hs"
  putNewLn ()
  --
  putStrLn "{- 2.3 Standard prelude -}"
  putStrLn "+ - * / ..."
  putStrLn "head tail !! take drop length"
  putStrLn "sum product ++ reverse "
  putNewLn ()
  --
  putStrLn "{- 2.4 Function application -}"
  putStrLn "f x"
  putNewLn ()
  --
  putStrLn "{- 2.5 Haskell scripts -}"
  putStrLn ":type expr"
  putStrLn ":?"
  putStrLn ":q"
  putStrLn ("quadruple 10 = " ++ show (quadruple 10))
  putStrLn ("take (double 2) [1,2,3,4,5] = " ++ show (take (double 2) [1,2,3,4,5]))
  putStrLn ("factorial 10 = " ++ show (factorial 10))
  putStrLn ("average [1,2,3,4,5] = " ++ show (average [1,2,3,4,5]))
  putNewLn ()
  --
  putStrLn "{- ex 2.1 Work through the examples from this chapter -}"
  putNewLn ()
  --
  putStrLn "{- ex 2.2 -}"
  putStrLn ("2^3*4 = " ++ show ((2 ^ 3) * 4))
  putStrLn ("2*3+4*5 = " ++ show ((2 * 3) + (4 * 5)))
  putStrLn ("2+3*4^5 = " ++ show (2 + (3 * (4 ^ 5))))
  putNewLn ()
  --
  putStrLn "{- ex 2.3 -}"
  putStrLn ("n = " ++ show n)
  putNewLn ()
  --
  putStrLn "{- ex 2.4 -}"
  putStrLn ("lastSelect [4,3,2,1] = " ++ show (lastSelect [4,3,2,1]))
  putStrLn ("lastRevHead [4,3,2,1] = " ++ show (lastRevHead [4,3,2,1]))
  putStrLn ("lastRecur [4,3,2,1] = " ++ show (lastRecur [4,3,2,1]))
  putNewLn ()
  --
  putStrLn "{- ex 2.5 -}"
  putStrLn ("initTake [4,3,2,1] = " ++ show (initTake [4,3,2,1]))
  putStrLn ("initRev [4,3,2,1] = " ++ show (initRev [4,3,2,1]))
  putStrLn ("initRecur [4,3,2,1] = " ++ show (initRecur [4,3,2,1]))
