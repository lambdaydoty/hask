-- type IO a = World -> (a,World)
-- type IO () = World -> ((),World)
-- main :: IO ()

-- getChar :: World -> (Char,World)        | IO Char
-- putChar :: Char -> World -> ((),World)  | Char -> IO ()
-- return :: a -> IO a                     | a -> IO a
--
act :: IO (Char,Char)
act = do x <- getChar
         y <- getChar
         z <- getChar
         return (x,z)

main = getLine
