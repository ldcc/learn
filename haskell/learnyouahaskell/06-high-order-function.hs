-- high order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' (f) xs ys)

flip'1 :: (a -> b -> c) -> (b -> a -> c)
flip'1 f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs)
    | f x       = x : filter' f xs 
    | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSort ++ [x] ++ biggerSort
    where smallerSort = quicksort (filter' (<= x) xs)
          biggerSort  = quicksort (filter' (>  x) xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' f (x:xs) 
    | f x = x : takeWhile' f xs
    | otherwise = []

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | odd n  = n : chain (n * 3 + 1)
    | even n = n : chain (div n 2)

numLongChains1 :: Int
numLongChains1 = length (filter' isLong (map' chain [1..100]))   
    where isLong xs = length xs > 15

-- lambda + currying
numLongChains2 :: Int
numLongChains2 = length (filter' (\ xs -> length xs > 15) (map' chain [1..100]))

addThree1 :: (Num a) => a -> a -> a -> a   
addThree1 x y z = x + y + z

addThree3 :: (Num a) => a -> a -> a -> a   
addThree3 = \x y z -> x + y + z

addThree2 :: (Num a) => a -> a -> a -> a   
addThree2 = \x -> \y -> \z -> x + y + z

flip'2 :: (a -> b -> c) -> (b -> a -> c)
flip'2 f = \x y -> f y x

-- fold
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "empty list"
foldl1' f (acc:list) = iter f acc list
    where iter _ acc []     = acc
          iter f acc (x:xs) = iter f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "empty list"
foldr1' f list = iter f list list
    where iter _ _ [x]      = x
          iter f acc (x:xs) = f x (iter f acc xs)

sum' :: (Num a) => [a] -> a
sum' = foldr1' (+)

product' :: (Num a) => [a] -> a
product' = foldl1' (*)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl' (\acc x -> acc || x == y) False

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1' max

reverse' :: [a] -> [a]
reverse' = foldl' (flip'1 (:)) []

map'1 :: (a -> b) -> [a] -> [b]
map'1 f = foldr' (\x acc -> f x : acc) []

filter'1 :: (a -> Bool) -> [a] -> [a]
filter'1 f = foldl' (\acc x -> if f x then x : acc else acc) []

-- scan
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ acc   []   = [acc]
scanl' f acc (x:xs) = acc : (scanl' f (f acc x) xs)

scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' _ [] = error "empty list"
scanl1' f (acc:list) = iter f acc list
    where iter _ acc []     = [acc]
          iter f acc (x:xs) = acc : iter f (f acc x) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ acc []     = [acc]
scanr' f acc (x:xs) = f x (head tar) : tar
    where tar = scanr' f acc xs

scanr1' :: (a -> a -> a) -> [a] -> [a]
scanr1' _ [] = error "empty list"
scanr1' f list = iter f list list
    where iter _ _   [x]    = [x]
          iter f acc (x:xs) = f x (head tar) : tar
              where tar = iter f acc xs

-- $ & .
numLongChains3 :: Int
numLongChains3 = length $ filter' (> 15) $ map' (length . chain) [1..100]

elem'1 :: (Eq a) => a -> [a] -> Bool
elem'1 y = foldr' ((||) . (== y)) False

map'2 :: (a -> b) -> [a] -> [b]
map'2 f = foldr' ((:) . f) []

oddSquareSum :: Integer   
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]