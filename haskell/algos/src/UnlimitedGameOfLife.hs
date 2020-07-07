module UnlimitedGameOfLife where

-- The rules of the game are:
--
-- 1. Any live cell witeleh fewer than two live neighbours dies, as if caused by underpopulation.
-- 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
-- 3. Any live cell with two or three live neighbours lives on to the next generation.
-- 4. Any dead cell with exactly three live neighbours becomes a live cell.

import Data.List (delete, elemIndices)
import Data.Maybe (catMaybes)

neis = delete (0,0) [(x,y) | y <- [-1..1], x <- [-1..1]]

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration cosmos p = gen p
  where
    lattice x y = map (\ (a, b) -> cosmos ! (b+y) >>= (! (a+x))) neis
    gen 0 = cosmos
    gen n = getGeneration (next 0 0) (n-1)
    next :: Int -> Int -> [[Int]]
    next x y = case cosmos ! y of
      Nothing -> [[]]
      Just row -> case row ! x of
        Nothing -> [] : next 0 (y+1)
        Just pos -> let (nr:ncsms) = next (x+1) y in ((pass (lattice x y) pos) : nr) : ncsms
    pass :: [Maybe Int] -> Int -> Int
    pass neighbours locate
      | liveNeighbour < 2 = 0
      | liveNeighbour > 3 = 0
      | liveNeighbour == 3 && locate == 0 = 1
      | liveNeighbour == 2 && liveNeighbour == 3 = 1
      | otherwise = 0
      where liveNeighbour = length . elemIndices 1 $ catMaybes neighbours

(!) :: [a] -> Int -> Maybe a
xs ! n
  | n < 0 = Nothing
  | n >= length xs = Nothing
  | otherwise = Just $ xs !! n
