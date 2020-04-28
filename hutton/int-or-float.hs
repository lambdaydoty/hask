type IntOrFloat = Either Int Float

asFloat :: IntOrFloat -> Float
-- asFloat (Left x)  = fromIntegral x
-- asFloat (Right x) = x
asFloat = either fromIntegral id

main = do
  print (asFloat (Left 42))
  print (asFloat (Right 42.0))
