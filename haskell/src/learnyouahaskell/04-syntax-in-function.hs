lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY!"
lucky x = "Sorry..."

sayMe :: (Integral a) => a -> String
sayMe 1 = "ONE!"
sayMe 2 = "TWO!"
sayMe 3 = "THREE!"
sayMe 4 = "FOUR!"
sayMe 5 = "FIVE!"
sayMe x = "No match 1 to 5..."

factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a)  => (a, a) -> (a, a) -> (a, a)
-- don't do this
-- addVectors a b = (fst a + fst b, snd a + snd b)
-- but this
addVectors (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

-- Triad 
first  :: (a, b, c) -> a
first  (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third  :: (a, b, c) -> c
third  (_, _, z) = z

pairs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
nums = [1, 2, 3, 4, 5, 6]

car' :: [a] -> a
car' [] = error "can't call head on an empty list, dummy!"
car' (x : _) = x
cdr' :: [a] -> [a]
cdr' [] = error "can't call head on an empty list, dummy!"
cdr' (_: xs) = xs

assis :: [a] -> [a] -> [a]
assis (x : el) tar = assis el (x : tar)
assis [] tar = tar
reverse' :: [a] -> [a]
reverse' xs = assis xs []

cylinder :: (RealFloat a) => a -> a -> a   
cylinder r h =  
    let sideArea = 2 * pi * r * h   
        topArea  = pi * r ^2   
    in  sideArea + 2 * topArea

calcBims :: (RealFloat a) => [(a, a)] -> [a]
calcBims xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case expression
-- head' :: [a] -> a
-- head' [] = error "No head for empty list"
-- head' (x:_) = x

-- equivalence to
head' :: [a] -> a
head' xs = case xs of
    (x:_)     -> x
    otherwise -> error "No head for empty list"

--  i dont how to connect the following list behind the case expr
-- describeList :: [a] -> String 
-- describeList xs = "The list is " ++ case xs of
    -- []  -> "empty"
    -- [x] -> "a singleton list"
    -- xs  -> "a longer list"

-- sec way
describeList :: [a] -> String 
describeList xs = "The list is " ++ what xs ++ "."
    where what []  = "empty"
          what [x] = "a singleton list"
          what xs  = "a longer list"
