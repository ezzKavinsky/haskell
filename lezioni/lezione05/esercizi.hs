pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n], (x*x)+(y*y)==(n*n)]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x==sum (tail(reverse(factors x)))]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]