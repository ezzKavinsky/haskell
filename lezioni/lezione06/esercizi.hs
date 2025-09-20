{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (elem, (!!), replicate, concat, and)
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x 
(!!) (_:xs) n = (!!) xs (n-1)

elem :: Eq a => a -> [a] -> Bool
elem e (x:xs) | e == x    = True
              | null xs   = False
              | otherwise = elem e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | x<=y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = msort(take half xs) `merge` msort(drop half xs)
  where half = length xs `div` 2