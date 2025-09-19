{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
abs :: Int -> Int
abs n = if n>=0 then n else -n  -- abs restituisce n se n é non negativo, -n altrimenti

signum :: Int -> Int
signum n = if n<0 then -1 else if n == 0 then 0 else 1

abs2 :: (Ord a, Num a) => a -> a
abs2 n | n>=0 = n
       | otherwise = -n

signum2 :: (Ord a1, Num a1, Num a2) => a1 -> a2
signum2 n | n<0 = -1
          | n==0 = 0
          | otherwise = 1

-- Funzioni presenti nella libreria
not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

add :: Int -> (Int -> Int)
add = \ x -> (\y -> x+y)

odds :: (Num b, Enum b) => b -> [b]
odds n = map (\ x -> x*2 + 1) [0..n-1]