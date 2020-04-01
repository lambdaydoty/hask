safeTail :: [Int] -> Maybe [Int]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

sq :: Int -> Int
sq x = x * x

main = do
  print (fmap (fmap sq) (Just [1, 2, 3]))
  print (safeTail [])
  print (safeTail [1, 2, 3])
