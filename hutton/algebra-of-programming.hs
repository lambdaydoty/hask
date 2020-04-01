-- 1.1 Datatypes

data Bool = False | True
  deriving Show

data Char = Ascii0 | Ascii1 | Ascii2 | Ascii3 | Ascii4
  deriving Show

data Either = Bool Main.Bool | Char Main.Char
  deriving Show

type Both = (Main.Bool,Main.Char)

x :: Main.Bool
x = Main.True

y :: Main.Either
y = Char Main.Ascii0

z :: Main.Either
z = Bool Main.False

u :: Main.Both
u = (Main.True,Main.Ascii4)

not :: Main.Bool -> Main.Bool
not Main.True  = Main.False
not Main.False = Main.True

data Maybe a = Nothing | Just a
  deriving Show

m :: Main.Maybe Main.Char
m = Main.Nothing

main = do
  print x
  print y
  print z
  print u
  print (Main.not x)
  print m
