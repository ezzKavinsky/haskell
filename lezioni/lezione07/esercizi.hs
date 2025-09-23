map f = foldr(\x xs -> f x : xs) []

filter p = foldr(\x xs -> if p x then x:xs else xs) []