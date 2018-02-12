-- module list

member' :: (Eq a) => a -> [a] -> Bool
member' _ []    = False
member' a (x:xs) 
    | a == x    = True
    | otherwise = member' a xs

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs)
    | member' x tar = tar
    | otherwise     = x:tar
    where tar = nub' xs

intersperse' :: a -> [a] -> [a]
intersperse' _ []     = []
intersperse' _ [x]    = [x]
intersperse' a (x:xs) = x : a : (intersperse' a xs)

-- like ++
pp :: [a] -> [a] -> [a]
pp [] l     = l
pp l []     = l
pp (x:xs) l = x : xs `pp` l

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ []     = []
intercalate' _ [x]    = x
intercalate' a (x:xs) = x `pp` a `pp` (intercalate' a xs)

-- enhanced head
heads' :: [[a]] -> [a]
heads' []      = []
heads' ([]:xs) = heads' xs
heads' (x:xs)  = (head x) : (heads' xs) 

-- enhanced tail
tails' :: [[a]] -> [[a]]
tails' []       = []
tails' ([]:xs)  = tails' xs
tails' ([x]:xs) = tails' xs
tails' (x:xs)   = (tail x) : (tails' xs) 

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' l  = heads' l : (transpose' $ tails' l)