-- Float
-- Int          (limited-precision)
-- Integer      (unlimited-precision)
-- (_,_)        (pair)
-- f x          (application of f to x)
-- logBase 2 _  (lg _)
-- logBase e _  (ln _)
-- logBase 10 _ (log _)
-- log _        (ln _)
--

-- 1.3 Example: common words
-- text/string = list of characters:
--  visible: 'B', ',', ...
--  blank: ' ', '\n'
commonWords :: Int -> [Char] -> [Char]
-- what is a `word`?
-- what is the equality of words? ("hello", "Hello", "Hello!")
-- how to count words?
-- word = maximal sequence of characters not containing blanks ...-> split the text by blanks!
type Text = [Char]
type Word = [Char]
words :: Text -> [Word]
-- for equality:
toLower :: Char -> Char -- map toLower :: Text -> Text
-- how to count words:
sortWords :: [Word] -> [Word]
countRuns :: [Word] -> [(Int,Word)]
sortRuns :: [(Int,Word)] -> [(Int,Word)]
-- take the first n elements of the result
take :: Int -> [a] -> [a]
-- output
showRun :: (Int,Word) -> String -- map showRun :: [(Int,Word)] -> [String]
concat :: [[a]] -> [a]

commonWords n = concat . map showRun . take n .
  sortRuns . countRuns . sortWords .
    words . map toLower


main = do
  print (cos 0.739)
  print (logBase 2 2)
  print (log 2.71828)
