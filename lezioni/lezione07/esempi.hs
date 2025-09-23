{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Prelude hiding (dropWhile, takeWhile, or, and, (.), foldr)
{-# HLINT ignore "Use foldr" #-}
twice :: (a-> a) -> a -> a
twice f x = f(f x)

map :: (a -> b) -> [a] -> [b]
--map (+1) [1,3,5,7]  --esempio ---> [2,4,6,8]
map f xs = [f x | x<-xs] --versione con list comprehension
--map f [] = []
--map f (x:xs) = f x : map f xs --versione ricorsiva

filter :: (a -> Bool) -> [a] -> [a]
--filter even [1..10] --esempio ---> [2,4,6,8,10]
filter p xs = [x | x<-xs, p x]
--filter p [] = []
--filter p (x:xs) | p x = x : filter p xs
--                |otherwise = filter p xs

sum ::Num a => [a] -> a
--sum [] = 0
--sum (x:xs) = x + sum xs
sum = foldr (+) 0

product :: Num a => [a] -> a
--product [] = 1
--product (x:xs) = x*product xs
product = foldr (*) 1

or :: [Bool] -> Bool
--or [] = False
--or (x:xs) = x || or xs
or = foldr (||) False

and :: [Bool] -> Bool
--and [] = True
--and (x:xs) = x && and xs
and = foldr (&&) True

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

--foldr (+) 0 [1,2,3] ---> 1 + (2 + (3 + 0))
--foldr (*) 1 [1,2,3] ---> 1 * (2 * (3 * 1))

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

reverse :: [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

--esempio di composizione di funzioni
odd :: Int -> Bool
odd = not . even --odd x = not (even x)

all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]
--all even [2,4,6] ---> True
--all even [2,3,6] ---> False

any :: (a -> Bool) -> [a] -> Bool
any p xs = or [p x | x <-xs]
--any even [1,3,5] ---> False
--any even [1,4,5] ---> True

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = []
--takeWhile (<3) [1,2,3,4,1,2,3,4] ---> [1,2]
--takeWhile (<5) [1,2,3,4,1,2,3,4] ---> [1,2,3,4,1,2,3,4]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = x:xs
--dropWhile (<3) [1,2,3,4,1,2,3,4] ---> [3,4,1,2,3,4]
--dropWhile (<5) [1,2,3,4,1,2,3,4] ---> []
--dropWhile (<0) [1,2,3,4,1,2,3,4] ---> [1,2,3,4,1,2,3,4]
