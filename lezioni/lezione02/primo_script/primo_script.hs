double :: Int -> Int
double x = x + x -- Restituisce il doppio di x

quadruple :: Int -> Int
quadruple x = double (double x) -- Restituisce il quadruplo di x

miasomma :: [Int] -> Int
miasomma = sum --alias della funzione sum

take2 :: [a] -> [a]
take2 = take 2 --funzione take parzialmente istanziata in quanto il primo argomento è stato fissato a 2

factorial :: Int -> Int
factorial n = product [1..n] -- Restituisce il fattoriale di n

average :: [Int] -> Int
average ns = sum ns `div` length ns -- Restituisce la media intera di una lista di numeri interi

n :: Int --espressione di tipo Int
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

last :: [a] -> a
last xs = head (reverse xs) -- Restituisce l'ultimo elemento di una lista

last1 :: [a] -> [a]
last1 xs = drop (length xs - 1) xs -- Restituisce l'ultimo elemento di una lista

last2 :: [a] -> a
last2 xs = xs !! (length xs -1) -- Restituisce l'ultimo elemento di una lista

last3 :: [a] -> a
last3 [] = error "la lista è vuota"
last3 [x] = x
last3 (_:xs) = last3 xs -- Restituisce l'ultimo elemento di una lista

init :: [a] -> [a]
init xs = reverse (tail (reverse xs)) -- Restituisce tutti gli elementi di una lista tranne l'ultimo

init1 :: [a] -> [a]
init1 xs = take(length xs -1) xs -- Restituisce tutti gli elementi di una lista tranne l'ultimo

init2 :: [a] -> [a]
init2 [] = error "lista vuota"
init2 [x] = []
init2 (x:xs) = x : init2 xs