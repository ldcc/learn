module UnlimitedGameOfLife where

-- The rules of the game are:
--
-- 1. Any live cell wite fewer than two live neighbours dies, as if caused by underpopulation.
-- 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
-- 3. Any live cell with two or three live neighbours lives on to the next generation.
-- 4. Any dead cell with exactly three live neighbours becomes a live cell.
-- NOTICE: (return unfixed for every round matrix size)
--   The return value should be a 2d array cropped around all of the living cells.
--   If there are no living cells, then return [[]].

import Data.List (transpose)

-- neis = delete (0,0) [(x,y) | y <- [-1..1], x <- [-1..1]]
--
-- getGeneration :: [[Int]] -> Int -> [[Int]]
-- getGeneration cosmos p = gen p
--   where
--     col x y = [cosmos ! (b+y) >>= (! (a+x)) | (a,b) <- neis]
--     gen 0 = cosmos
--     gen n = getGeneration (next 0 0) (n-1)
--     next x y
--       | y >= (length $ head cosmos) = []
--       | x >= (length cosmos) = [] : next 0 (y+1)
--       | otherwise = (pass (sum . catMaybes $ col x y) (cosmos !! y !! x) : nr) : ncsms
--           where (nr:ncsms) = next (x+1) y
--     pass 3 _ = 1
--     pass 2 1 = 1
--     pass _ _ = 0
--
-- (!) :: [a] -> Int -> Maybe a
-- xs ! n
--   | n < 0 = Nothing
--   | n >= length xs = Nothing
--   | otherwise = Just $ xs !! n

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration = (!!) . iterate step
  where
    step = crop . (zipzip bio <*> sums) . pads
    sums = zipzip (+) <$> map sides <*> t . map sides . t . map three
    sides = zipWith (+) <$> r0 . r <*> r . r0 -- a cosmos shift and add it both side, r . r0 == lsf, r0 . r == rsf
    three = zipWith (+) <*> sides -- lsf + rsf + id, add
    pad x = r . (x:) . r . (x:)
    pads = (flip pad <*> flip replicate 0 . hl) . map (pad 0)
    crop = t . r . d . r . d . t . r . d . r . d
    d = dropWhile (all (==0))
    r = reverse
    t = transpose
    r0 = (0:) . r . tail
    zipzip = zipWith . zipWith
    hl [] = 0
    hl (x:_) = length x
    bio 1 2 = 1
    bio _ 3 = 1
    bio _ _ = 0