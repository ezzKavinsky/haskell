{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail1 :: [a] -> [a]
safetail1 [] = []
safetail1 (_:xs) = xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

(||) :: Bool -> Bool -> Bool
True || _ = True
True || b = True
b || True = True

(&&) :: Bool -> Bool -> Bool
(&&) x y = if not x Prelude.|| not y then False else True

--(&&) :: Bool -> Bool -> Bool
--(&&) x y = if x then True else False