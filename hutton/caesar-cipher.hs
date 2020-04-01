{-# LANGUAGE TemplateHaskell #-}
import           Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
        0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
        6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

main = do
  print (let2int 'a')
  print (int2let 0)
  print (shift 3 'a')
  print (shift 3 'z')
  print (shift (-3) 'c')
  print (shift 3 ' ')
  print (encode 3 "haskell is fun")
  print (encode (-3) "kdvnhoo lv ixq")
  print (percent 5 15)
  print (freqs "abbcccddddeeeee")


