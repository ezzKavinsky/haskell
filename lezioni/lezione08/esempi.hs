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