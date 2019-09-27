module MergeSort where

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = sort xs ++ ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = a2 ++ b2
  where
    a2 = sort [a | a <- xs, a <= x]
    b2 = x : sort [b | b <- xs, b > x]