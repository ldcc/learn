-- maximum'
maximum' :: (Ord a) => [a] -> a
maximum' []  = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x

--  take
take' :: (Num i, Ord i, Eq a) => i -> [a] -> [a]
take' n x
    | n <= 0   = []
    | x == []  = []
take' n (x:xs) = x : take' (n - 1) xs

-- reverse
reverse'1 :: [a] -> [a]
reverse'1 []     = []
reverse'1 (x:xs) = reverse'1 xs ++ [x]

reverse'2 :: [a] -> [a]
reverse'2 list = iter list []
    where iter []     nlist = nlist
          iter (x:xs) nlist = iter xs (x:nlist)

-- repeat
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip
zip'1 :: [a] -> [b] -> [(a,b)]
zip'1 _ [] = []
zip'1 [] _ = []
zip'1 (x:xs) (y:ys) = (x,y):zip'1 xs ys

zip'2 :: [a] -> [b] -> [(a,b)]
zip'2 a b = iter a b []
    where iter _ [] tar = tar
          iter [] _ tar = tar
          iter (x:xs) (y:ys) tar = iter xs ys (tar++[(x,y)])

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSort ++ [x] ++ biggerSort
    where smallerSort = quicksort [a | a <- xs, a <= x]
          biggerSort  = quicksort [a | a <- xs, a >  x]
