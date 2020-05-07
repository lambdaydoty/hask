double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

--
--

lastSelect = (!!) <*> ((subtract 1) . length)
lastRevHead = head . reverse
lastRecur (x:[]) = x
lastRecur (x:xs) = lastRecur xs

initTake = (flip take) <*> ((subtract 1) . length)
initRev = reverse . tail . reverse
initRecur (x:[]) = []
initRecur (x:xs) = x : initRecur(xs)

putNewLn () = putStrLn "\n"

main = do
  putStrLn "{- 2.5 Haskell scripts -}"
  putStrLn ("quadruple 10 = " ++ show (quadruple 10))
  putStrLn ("take (double 2) [1,2,3,4,5] = " ++ show (take (double 2) [1,2,3,4,5]))
  putStrLn ("factorial 10 = " ++ show (factorial 10))
  putStrLn ("average [1,2,3,4,5] = " ++ show (average [1,2,3,4,5]))
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
