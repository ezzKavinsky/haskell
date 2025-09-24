{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use ==" #-}
import Prelude hiding ((/=), (==), Bool, String, Just, Nothing, Maybe)
type String = [Char]

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left(x,y) = (x-1, y)

type Pair a = (a,a)

mult :: Pair Int -> Int
mult (x,y) = x * y

copy :: a -> Pair a
copy x = (x,x)

type Trans = Pos -> Pos

data Bool = False | True

data Answer = Yes | No | Unknown
answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--Circle :: Float -> Shape
--Rect :: Float -> Float -> Shape

data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat (n-1))

add :: Nat -> Nat -> Nat
--add m n = int2nat(nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

data Expr = Val Int
            | Add Expr Expr
            | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
-- eval = folde id (+) (*)
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
-- eval (Mul (Add (Val 2) (Val 3)) (Val 4))
-- eval (Mul (Val 5) (Val 4))

newtype Name = Name String

class Example a where
    f1 :: a -> a -> String
    f2 :: a -> a
    f3 :: a

instance Example Int where
    f1 x y = show $ (+) x y
    f2 = (+1)
    f3 = 0

--class Graphical a where
--    shape :: a -> Graphics

--instance Graphical Box where
--    shape = boxDraw --previosly defined    

--instance Graphical a => Graphical [a] where
--    shape = (foldr1 overGraphic) . (map shape)

--class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x /= y = not (x == y)
--    x == y = not (x /= y)

data Season = Spring | Summer | Autumn | Winter
    deriving (Eq, Ord, Enum, Show, Read)

notWinter = [Spring, Summer, Autumn]