{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Prelude hiding ((++), drop, zip, reverse, length, product)
{-# HLINT ignore "Use foldr" #-}
fac :: Int -> Int
--fac n = product [1..n]  --funzione fattoriale definita utilizzando la funzione prodotto
fac 0 = 1
fac n = n* fac(n-1) --funzione fattoriale definita in modo ricorsivo

product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * product ns

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

qsort :: Ord a => [a] -> [a]
qsort [x] = [x] 
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]